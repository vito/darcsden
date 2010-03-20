{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository.Forks where

import Darcs.CommandsAux (check_paths)
import Darcs.Hopefully (hopefully)
import Darcs.Patch (patch2patchinfo)
import Darcs.Patch.Depends (get_common_and_uncommon)
import Darcs.Patch.Info (make_filename)
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
import Darcs.Utils (withCurrentDirectory)
import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Sealed
import Data.Data (Data)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Happstack.State
import Text.StringTemplate (ToSElem(toSElem))
import Text.StringTemplate.Classes (SElem(SM))
import qualified Data.Map as M

import DarcsDen.Handler.Repository.Changes
import DarcsDen.Handler.Repository.Util
import DarcsDen.State.Repository
import DarcsDen.State.Util

data Fork = Fork Repository [PatchLog]
            deriving (Data, Typeable)

instance ToSElem Fork where
  toSElem (Fork r c) = SM (M.fromList [ ("repo", toSElem r)
                                      , ("changes", toSElem c)
                                      ])

getForkChanges :: Repository -> IO Fork
getForkChanges r = do Just rParent <- query (GetRepository (fromJust (rForkOf r)))
                      let pdir = repoDir (rOwner rParent) (rName rParent)
                          cdir = repoDir (rOwner r) (rName r)

                      Right pr <- getRepo pdir
                      Right cr <- getRepo cdir

                      pps <- read_repo pr
                      cps <- read_repo cr

                      let (_, _ :\/: them) = get_common_and_uncommon (pps, cps)
                          new = mapRL hopefully them

                      cs <- mapM (toLog . patch2patchinfo) new

                      return $ Fork r cs

mergePatches :: Repository -> [String] -> IO ()
mergePatches r ps
  = do Just rParent <- query (GetRepository (fromJust (rForkOf r)))

       let pdir = repoDir (rOwner rParent) (rName rParent)
           cdir = repoDir (rOwner r) (rName r)

       withCurrentDirectory pdir $ withRepoLock [] $- \pr -> do
         cr <- identifyRepositoryFor pr cdir

         pps <- read_repo pr
         cps <- read_repo cr

         let (_, us :\/: them) = get_common_and_uncommon (pps, cps)
             chosen = filterFL (\p -> if take 20 (make_filename (patch2patchinfo (hopefully p))) `elem` ps
                                         then NotEq
                                         else IsEq) (reverseRL them)

         check_paths [] chosen

         Sealed pw <- tentativelyMergePatches pr "pull" [] (reverseRL us) chosen
         invalidateIndex pr
         withGutsOf pr $ do finalizeRepositoryChanges pr
                            applyToWorking pr [] pw
