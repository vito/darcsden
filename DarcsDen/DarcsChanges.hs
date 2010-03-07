{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# LANGUAGE CPP, PatternGuards #-}

module DarcsDen.DarcsChanges where
import Prelude hiding ( log )

import Data.List ( intersect, sort )
import Data.Maybe ( fromMaybe )
import Control.Monad ( when, unless )

import Darcs.Hopefully ( hopefullyM, info )
import Darcs.Patch.Depends ( slightly_optimize_patchset )
import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias )
import Darcs.Arguments ( DarcsFlag(Context, HumanReadable, MachineReadable,
                                   Interactive, Count,
                                   NumberPatches, XMLOutput, Summary,
                                   Verbose, Debug),
                         fixSubPaths, changesFormat,
                         possiblyRemoteRepoDir, getRepourl,
                         workingRepoDir, onlyToFiles,
                         summary, changesReverse,
                         matchSeveralOrRange,
                         matchMaxcount, maxCount,
                         allInteractive, showFriendly,
                         networkOptions
                      )
import Darcs.Flags ( doReverse, showChangesOnlyToFiles )
import Darcs.RepoPath ( toFilePath, rootDirectory )
import Darcs.Patch.FileName ( fp2fn, fn2fp, norm_path )
import Darcs.Repository ( Repository, PatchSet, PatchInfoAnd,
                          withRepositoryDirectory, ($-), findRepository,
                          read_repo, unrecordedChanges )
import Darcs.Patch.Info ( to_xml, showPatchInfo )
import Darcs.Patch.Depends ( get_common_and_uncommon )
import Darcs.Patch.TouchesFiles ( look_touch )
import Darcs.Patch ( RepoPatch, invert, xmlSummary, description, applyToFilepaths,
                     listTouchedFiles, effect, identity )
import Darcs.Witnesses.Ordered ( (:\/:)(..), RL(..), unsafeFL, unsafeUnRL, concatRL,
                             EqCheck(..), filterFL )
import Darcs.Match ( firstMatch, secondMatch,
               matchAPatchread, haveNonrangeMatch,
               matchFirstPatchset, matchSecondPatchset,
             )
import Darcs.Commands.Annotate ( createdAsXml )
import Printer ( Doc, putDocLnWith, simplePrinters, (<+>),
                 renderString, prefix, text, vcat, vsep,
                 ($$), empty, errorDoc, insert_before_lastline )
import Darcs.ColorPrinter ( fancyPrinters )
import Progress ( setProgressMode, debugMessage )
import Darcs.SelectChanges ( view_changes )
import Darcs.Witnesses.Sealed ( unsafeUnseal )


changesCmd :: [DarcsFlag] -> [String] -> IO ()
changesCmd [Context _] [] = return ()
changesCmd opts args | Context rootDirectory `elem` opts =
  let repodir = fromMaybe "." (getRepourl opts) in
  withRepositoryDirectory opts repodir $- \repository -> do
  when (args /= []) $ fail "changes --context cannot accept other arguments"
  changesContext repository opts
changesCmd opts args =
  let repodir = fromMaybe "." (getRepourl opts) in
  withRepositoryDirectory opts repodir $- \repository -> do
  unless (Debug `elem` opts) $ setProgressMode False
  files <- sort `fmap` fixSubPaths opts args
  unrec <- if null files then return identity
             else unrecordedChanges opts repository files
           `catch` \_ -> return identity -- this is triggered when repository is remote
  let filez = map (fn2fp . norm_path . fp2fn) $ applyToFilepaths (invert unrec) $ map toFilePath files
      filtered_changes p = maybe_reverse $ getChangesInfo opts filez p
  debugMessage "About to read the repository..."
  patches <- read_repo repository
  debugMessage "Done reading the repository."
  if Interactive `elem` opts
    then do let (fp_and_fs, _, _) = filtered_changes patches
                fp = map fst fp_and_fs
            view_changes opts (unsafeFL fp)
    else do when (not (null files) && not (XMLOutput `elem` opts)) $
                 putStrLn $ "Changes to "++unwords filez++":\n"
            debugMessage "About to print the changes..."
            let printers = if XMLOutput `elem` opts then simplePrinters else fancyPrinters
            ps <- read_repo repository -- read repo again to prevent holding onto
                                       -- values forced by filtered_changes
            putDocLnWith printers $ changelog opts ps $ filtered_changes patches
  where maybe_reverse (xs,b,c) = if doReverse opts
                                 then (reverse xs, b, c)
                                 else (xs, b, c)


getChangesInfo :: RepoPatch p => [DarcsFlag] -> [FilePath] -> PatchSet p
                 -> ([(PatchInfoAnd p, [FilePath])], [FilePath], Doc)
getChangesInfo opts plain_fs ps =
  case get_common_and_uncommon (p2s,p1s) of
  (_,us:\/:_) -> filterPatchesByNames (maxCount opts) fs $ filter pf $ unsafeUnRL us
  where fs = map (\x -> "./" ++ x) $ plain_fs
        p1s = if firstMatch opts then unsafeUnseal $ matchFirstPatchset opts ps
                                  else NilRL:<:NilRL
        p2s = if secondMatch opts then unsafeUnseal $ matchSecondPatchset opts ps
                                   else ps
        pf = if haveNonrangeMatch opts
             then matchAPatchread opts
             else \_ -> True

-- | Take a list of filenames and patches and produce a list of
-- patches that actually touch the given files with list of touched
-- file names, a new file list that represents the same set of files
-- as in input, before the returned patches would have been applied,
-- and possibly an error. Additionaly, the function takes a "depth
-- limit" -- maxcount, that could be Nothing (return everything) or
-- "Just n" -- returns at most n patches touching the file (starting
-- from the beginning of the patch list).
filterPatchesByNames :: RepoPatch p =>
                           Maybe Int -- ^ maxcount
                        -> [FilePath] -- ^ filenames
                        -> [PatchInfoAnd p] -- ^ patchlist
                        -> ([(PatchInfoAnd p,[FilePath])], [FilePath], Doc)
filterPatchesByNames (Just 0) _ _ = ([], [], empty)
filterPatchesByNames _ _ [] = ([], [], empty)
filterPatchesByNames maxcount [] (hp:ps) =
    (hp, []) -:- filterPatchesByNames (subtract 1 `fmap` maxcount) [] ps
filterPatchesByNames maxcount fs (hp:ps)
    | Just p <- hopefullyM hp =
    case look_touch fs (invert p) of
    (True, []) -> ([(hp, fs)], fs, empty)
    (True, fs') -> (hp, fs) -:- filterPatchesByNames
                                (subtract 1 `fmap` maxcount) fs' ps
    (False, fs') -> filterPatchesByNames maxcount fs' ps
filterPatchesByNames _ _ (hp:_) =
    ([], [], text "Can't find changes prior to:" $$ description hp)

-- | Note, lazy pattern matching is required to make functions like
-- filterPatchesByNames lazy in case you are only not interested in
-- the first element. E.g.:
--
--   let (fs, _, _) = filterPatchesByNames ...
(-:-) :: a -> ([a],b,c) -> ([a],b,c)
x -:- ~(xs,y,z) = (x:xs,y,z)

changelog :: RepoPatch p => [DarcsFlag] -> PatchSet p -> ([(PatchInfoAnd p, [FilePath])], [FilePath], Doc)
          -> Doc
changelog opts patchset (pis_and_fs, orig_fs, errstring)
    | Count `elem` opts = text $ show $ length pis_and_fs
    | MachineReadable `elem` opts =
        if renderString errstring == ""
        then vsep $ map (showPatchInfo.info) pis
        else errorDoc errstring
    | XMLOutput `elem` opts =
         text "<changelog>"
      $$ vcat xml_file_names
      $$ vcat actual_xml_changes
      $$ text "</changelog>"
    | Summary `elem` opts || Verbose `elem`  opts =
           vsep (map (number_patch change_with_summary) pis_and_fs)
        $$ errstring
    | otherwise = vsep (map (number_patch description') pis_and_fs)
               $$ errstring
    where change_with_summary (hp, fs)
              | Just p <- hopefullyM hp = if showChangesOnlyToFiles opts
                                          then description hp $$ text "" $$
                                               indent (showFriendly opts (filterFL xx $ effect p))
                                          else showFriendly opts p
              | otherwise = description hp
                            $$ indent (text "[this patch is unavailable]")
              where xx x = case listTouchedFiles x of
                             ys | null $ ys `intersect` fs -> IsEq
                             _ -> NotEq
          xml_with_summary hp
              | Just p <- hopefullyM hp = insert_before_lastline
                                           (to_xml $ info hp) (indent $ xmlSummary p)
          xml_with_summary hp = to_xml (info hp)
          indent = prefix "    "
          actual_xml_changes = if Summary `elem` opts
                               then map xml_with_summary pis
                               else map (to_xml.info) pis
          xml_file_names = map (createdAsXml first_change) orig_fs
          first_change = if doReverse opts
                         then info $ head pis
                         else info $ last pis
          number_patch f x = if NumberPatches `elem` opts
                             then case get_number (fst x) of
                                  Just n -> text (show n++":") <+> f x
                                  Nothing -> f x
                             else f x
          get_number :: PatchInfoAnd p -> Maybe Int
          get_number y = gn 1 (concatRL patchset)
              where iy = info y
                    gn n (b:<:bs) | seq n (info b) == iy = Just n
                                  | otherwise = gn (n+1) bs
                    gn _ NilRL = Nothing
          pis = map fst pis_and_fs
          description' = description . fst

changesContext :: RepoPatch p => Repository p -> [DarcsFlag] -> IO ()
changesContext repository opts = do
  r <- read_repo repository
  putStrLn "\nContext:\n"
  when (not $ null (unsafeUnRL r) || null (unsafeUnRL $ head $ unsafeUnRL r)) $
    putDocLnWith simplePrinters $ changelog opts' NilRL $
                 getChangesInfo opts' []
                 (headRL (slightly_optimize_patchset r) :<: NilRL)
    where opts' = if HumanReadable `elem` opts || XMLOutput `elem` opts
                  then opts
                  else MachineReadable : opts
          headRL (x:<:_) = x
          headRL NilRL = undefined

