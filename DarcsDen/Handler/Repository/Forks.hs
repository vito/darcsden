{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository.Forks where

import Darcs.CommandsAux (check_paths)
import Darcs.Flags (DarcsFlag(SkipConflicts))
import Darcs.Hopefully (hopefully, info)
import Darcs.Patch (patch2patchinfo)
import Darcs.Patch.Depends (get_common_and_uncommon)
import Darcs.Patch.Info (make_filename)
import Darcs.Patch.Permutations (partitionFL)
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
import DarcsDen.State.Session
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

mergePatches :: Repository -> [String] -> Session -> IO Bool
mergePatches r ps s
  = withCurrentDirectory orig $ withRepoLock [] $- \pr -> do
      cr <- identifyRepositoryFor pr fork

      pps <- read_repo pr
      cps <- read_repo cr

      let (_, us :\/: them) = get_common_and_uncommon (pps, cps)
          -- to include dependencies:
          -- { (_ :> revChosen) = partitionRL (\p -> ... `notElem` ps) them; chosen = reverseRL revChosen }
          (chosen :> _) = partitionFL (\p -> take 20 (make_filename (info p)) `elem` ps) (reverseRL them)

      (conflicts, Sealed merge) <- filterOutConflicts [SkipConflicts] us pr chosen

      if conflicts
         then do warn ("Patches for fork \"" ++ rOwner r ++ "/" ++ rName r ++ "\" could not be applied cleanly and have been skipped.") s
                 return False
         else do
      check_paths [] merge

      Sealed pw <- tentativelyMergePatches pr "pull" [] (reverseRL us) merge
      invalidateIndex pr
      withGutsOf pr $ do finalizeRepositoryChanges pr
                         applyToWorking pr [] pw

      Just p <- query (GetRepository parent)
      setRepoPermissions p
      setRepoPermissions r

      return True
  where parent = fromJust (rForkOf r)
        orig = uncurry repoDir parent
        fork = repoDir (rOwner r) (rName r)
