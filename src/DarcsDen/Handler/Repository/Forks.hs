module DarcsDen.Handler.Repository.Forks where

import Darcs.CommandsAux (check_paths)
import Darcs.Flags (DarcsFlag(SkipConflicts))
import Darcs.Patch.Depends (get_common_and_uncommon)
import Darcs.Hopefully (hopefully, info)
import Darcs.Patch.Info (make_filename)
import Darcs.Patch.Permutations
import Darcs.Repository
  ( ($-)
  , applyToWorking
  , finalizeRepositoryChanges
  , identifyRepositoryFor
  , invalidateIndex
  , read_repo
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

data Fork = Fork Repository [PatchLog]


getForkChanges :: Repository -> IO Fork
getForkChanges r = do
    Just rParent <- getRepositoryByID (fromJust (rForkOf r))
    let pdir = repoDir (rOwner rParent) (rName rParent)
        cdir = repoDir (rOwner r) (rName r)

    Right pr <- getRepo pdir
    Right cr <- getRepo cdir

    pps <- read_repo pr
    cps <- read_repo cr

    let (_, _ :\/: them) = get_common_and_uncommon (pps, cps)
        depends = findAllDeps (reverseRL them)
        cs = map (\p ->
            let l = toLog (hopefully p)
            in case lookup (make_filename (info p)) depends of
                Just ds -> l { pDepends = map (take 20 . make_filename . info) ds }
                Nothing -> l) (unsafeUnRL them)

    changes <- findUsers cs

    return $ Fork r changes

mergePatches :: Repository -> [String] -> Session -> IO Bool
mergePatches r ps s = do
  Just parent <- getRepositoryByID (fromJust (rForkOf r))
  withCurrentDirectory (origin parent) $ withRepoLock [] $- \pr -> do
      cr <- identifyRepositoryFor pr fork

      pps <- read_repo pr
      cps <- read_repo cr

      let (_, us :\/: them) = get_common_and_uncommon (pps, cps)
          (chosen :> _) = partitionFL ((`elem` ps) . take 20 . make_filename . info)
                                      (reverseRL them)

      (conflicts, Sealed merge) <- filterOutConflicts [SkipConflicts] us pr chosen

      if conflicts || lengthFL merge < length ps
         then do warn ("Patches for fork \"" ++ rOwner r ++ "/" ++ rName r ++ "\" could not be applied cleanly and have been skipped.") s
                 return False
         else do
      check_paths [] merge

      Sealed pw <- tentativelyMergePatches pr "pull" [] (reverseRL us) merge
      invalidateIndex pr
      withGutsOf pr $ do finalizeRepositoryChanges pr
                         applyToWorking pr [] pw

      return True
  where origin p = repoDir (rOwner p) (rName p)
        fork = repoDir (rOwner r) (rName r)
