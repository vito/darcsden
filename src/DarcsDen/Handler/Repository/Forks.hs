module DarcsDen.Handler.Repository.Forks where

import Darcs.CommandsAux (checkPaths)
import Darcs.Flags (DarcsFlag(SkipConflicts))
import Darcs.Patch.Depends (findUncommon)
import Darcs.Patch.Info (makeFilename)
import Darcs.Patch.Named (getdeps)
import Darcs.Patch.PatchInfoAnd (hopefully, info)
import Darcs.Patch.Permutations
import Darcs.Repository
    ( ($-)
    , applyToWorking
    , finalizeRepositoryChanges
    , identifyRepositoryFor
    , invalidateIndex
    , readRepo
    , tentativelyMergePatches
    , withGutsOf
    , withRepoLock
    )
import Darcs.SelectChanges (filterOutConflicts)
import Darcs.Utils (withCurrentDirectory)
import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Sealed
import Data.Maybe (fromJust)

import DarcsDen.Handler.Repository.Changes
import DarcsDen.Handler.Repository.Util
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.Util

data Fork =
    Fork
        { fRepo :: Repository
        , fPatches :: [PatchLog]
        }


getForkChanges :: Repository -> IO Fork
getForkChanges r = do
    Just rParent <- getRepositoryByID (fromJust (rForkOf r))
    let pdir = repoDir (rOwner rParent) (rName rParent)
        cdir = repoDir (rOwner r) (rName r)

    Right pr <- getRepo pdir
    Right cr <- getRepo cdir

    pps <- readRepo pr
    cps <- readRepo cr

    let cs =
            case findUncommon pps cps of
                _ :\/: them ->
                    let depends = map (\(p, ds) -> (makeFilename p, ds)) (findAllDeps (reverseFL them))
                    in
                        mapRL
                            (\p ->
                                let l = toLog (info p, getdeps (hopefully p))
                                in case lookup (makeFilename (info p)) depends of
                                    Just ds -> l
                                        { pDepends =
                                            map (take 20 . makeFilename) ds
                                        }
                                    Nothing -> l)
                            (reverseFL them)

    changes <- findUsers cs

    return $ Fork r changes

mergePatches :: Repository -> [String] -> Session -> IO Bool
mergePatches r ps s = do
    Just parent <- getRepositoryByID (fromJust (rForkOf r))
    withCurrentDirectory (origin parent) $ withRepoLock [] $- \pr -> do
        cr <- identifyRepositoryFor pr fork

        pps <- readRepo pr
        cps <- readRepo cr

        case findUncommon pps cps of
            us :\/: them ->
                case partitionFL ((`elem` ps) . take 20 . makeFilename . info) them of
                    chosen :> _ -> do
                        (conflicts, Sealed merge) <- filterOutConflicts [SkipConflicts] (reverseFL us) pr chosen

                        if conflicts || lengthFL merge < length ps
                            then do
                                flip warn s . unwords $
                                    [ "Patches for fork"
                                    , "\"" ++ rOwner r ++ "/" ++ rName r ++ "\""
                                    , "could not be applied cleanly and have been skipped."
                                    ]
                                return False
                            else do

                        checkPaths [] merge

                        Sealed pw <- tentativelyMergePatches pr "pull" [] us merge
                        invalidateIndex pr
                        withGutsOf pr $ do
                            finalizeRepositoryChanges pr
                            applyToWorking pr [] pw
                            return ()

                        return True
  where
    origin p = repoDir (rOwner p) (rName p)
    fork = repoDir (rOwner r) (rName r)
