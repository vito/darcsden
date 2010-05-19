{-# LANGUAGE GADTs #-}
module DarcsDen.Handler.Repository.Changes where

import Darcs.Patch.Info (pi_date, pi_name, pi_author, pi_log, make_filename)
import Darcs.Patch.FileName (fn2fp)
import Darcs.Patch.Patchy (Commute(..))
import Darcs.Patch.Prim (Prim(..), DirPatchType(..), FilePatchType(..))
import Darcs.Hopefully (PatchInfoAnd, info)
import Darcs.Witnesses.Ordered
import Data.List (nub)
import System.Time (calendarTimeToString)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Witnesses.Ordered as WO
import qualified Data.Map as M

import DarcsDen.Handler.Repository.Util
import DarcsDen.State.User
import DarcsDen.Util


data PatchLog = PatchLog { pID :: String
                         , pDate :: String
                         , pName :: String
                         , pAuthor :: String
                         , pIsUser :: Bool
                         , pLog :: [String]
                         , pDepends :: [String]
                         }
                deriving (Eq, Show)


data PatchChange = Moved { cmFrom :: FilePath
                         , cmTo :: FilePath
                         }
                 | DirChange { cdName :: FilePath
                             , cdType :: DirChange
                             }
                 | FileChange { cfName :: FilePath
                              , cfType :: FileChange
                              }
                 | PrefChange { cpName :: String
                              , cpFrom :: String
                              , cpTo :: String
                              }
                 deriving (Eq, Show)

data DirChange = DirRemoved | DirAdded
               deriving (Eq, Show)

data FileChange = FileRemoved
                | FileAdded
                | FileHunk { fchLine :: Int
                           , fchRemove :: String
                           , fchAdd :: String
                           }
                | FileBinary
                | FileReplace { fchFind :: String
                              , fchReplace :: String
                              }
                deriving (Eq, Show)

data PatchChanges = PatchChanges { pPatch :: PatchLog
                                 , pChanges :: [PatchChange]
                                 }
                    deriving (Eq, Show)


toLog :: P.Named p -> IO PatchLog
toLog p = do mu <- getUserByEmail (emailFrom (pi_author i))

             let (author, isUser) =
                     case mu of
                          Nothing -> (pi_author i, False)
                          Just u -> (uName u, True)

             return $ PatchLog (take 20 $ make_filename i)
                               (calendarTimeToString $ pi_date i)
                               (pi_name i)
                               author
                               isUser
                               (pi_log i)
                               (map (take 20 . make_filename) (P.getdeps p))
  where emailFrom = reverse . takeWhile (/= '<') . tail . reverse
        i = P.patch2patchinfo p

toChanges :: P.Effect p => P.Named p -> IO PatchChanges
toChanges p = do l <- toLog p
                 return . PatchChanges l . simplify [] . map primToChange . WO.unsafeUnFL $ P.effect p
  where simplify a [] = reverse a
        simplify a (c@(FileChange n t):cs) | t `elem` [FileAdded, FileRemoved, FileBinary]
          = simplify (c:filter (notFile n) a) (filter (notFile n) cs)
        simplify a (c@(FileChange _ (FileReplace _ _)):cs)
          = simplify (c:a) cs
        simplify a (FileChange n (FileHunk l f t):cs)
          = simplify (FileChange n (FileHunk l (hl n f) (hl n t)):a) cs
        simplify a (c@(PrefChange _ _ _):cs) = simplify (c:a) cs
        simplify a (_:cs) = simplify a cs

        notFile n (FileChange { cfName = n' }) | n == n' = False
        notFile _ _ = True

        hl _ "" = ""
        hl n t = highlight n t []

primToChange :: Prim -> PatchChange
primToChange (Move f t) = Moved (drop 2 $ fn2fp f) (drop 2 $ fn2fp t)
primToChange (DP f t) = DirChange (drop 2 $ fn2fp f) (fromDP t)
primToChange (FP f t) = FileChange (drop 2 $ fn2fp f) (fromFP t)
primToChange (ChangePref n f t) = PrefChange n f t
primToChange a = error ("primToChange not supported for " ++ show a)

fromDP :: DirPatchType -> DirChange
fromDP RmDir = DirRemoved
fromDP AddDir = DirAdded

fromFP :: FilePatchType -> FileChange
fromFP RmFile = FileRemoved
fromFP AddFile = FileAdded
fromFP (Hunk l rs as) = FileHunk l (unlines $ map fromBS rs) (unlines $ map fromBS as)
fromFP (Binary _ _) = FileBinary
fromFP (TokReplace _ f r) = FileReplace f r

getChanges :: String -> Int -> IO ([PatchLog], Int)
getChanges dir page = R.withRepositoryDirectory [] dir $ \dr ->
  do pset <- R.read_repo dr
     let ps = fromPS pset
     ls <- mapM toLog . paginate 30 page $ ps
     return (ls, ceiling ((fromIntegral (length ps) :: Double) / 30))

getPatch :: String -> String -> IO PatchChanges
getPatch dir patch = R.withRepositoryDirectory [] dir $ \dr ->
  do pset <- R.read_repo dr
     let ps = fromPS pset
         p = head $ filter (\p' -> patch == take 20 (P.patchname p')) ps

     toChanges p

fromPS :: P.RepoPatch p => R.PatchSet p -> [P.Named p]
fromPS = WO.unsafeUnRL . WO.reverseFL . R.patchSetToPatches

summarize :: [[(String, String)]] -> [PatchChange] -> [M.Map String String]
summarize a [] = map M.fromList $ nub a
summarize a (FileChange n FileRemoved:cs) = summarize (a ++ [[("removed", n)]]) cs
summarize a (FileChange n FileAdded:cs) = summarize (a ++ [[("added", n)]]) cs
summarize a (FileChange n (FileReplace f t):cs) = summarize (a ++ [[ ("replaced", n)
                                                                   , ("from", f)
                                                                   , ("to", t)
                                                                   ]]) cs
summarize a (FileChange n _:cs) = summarize (a ++ [[("modified", n)]]) cs
summarize a (PrefChange n f t:cs) = summarize (a ++ [[ ("preference", n)
                                                       , ("from", f)
                                                       , ("to", t)
                                                       , ("type", "change")
                                                       ]]) cs
summarize a (_:cs) = summarize a cs

isModification :: PatchChange -> Bool
isModification (FileChange _ (FileHunk _ _ _)) = True
isModification _ = False

-- The following is ported over from Camp.
findAllDeps :: Commute p => FL (PatchInfoAnd p) -> [(String, [PatchInfoAnd p])]
findAllDeps = f NilRL
    where f :: Commute p => RL (PatchInfoAnd p) -> FL (PatchInfoAnd p) -> [(String, [PatchInfoAnd p])]
          f _ NilFL = []
          f past (p :>: ps) = (make_filename (info p), findDeps past p) : f (p :<: past) ps

findDeps :: Commute p => RL (PatchInfoAnd p) -> PatchInfoAnd p -> [PatchInfoAnd p]
findDeps NilRL _ = []
findDeps (p :<: ps) me = case commute (p :> me) of
                            Just (me' :> _) -> findDeps ps me'
                            Nothing ->
                                case commuteOut ps p of
                                HiddenFrom ps' ->
                                    p : findDeps ps' me

data HiddenFrom seq p
    where HiddenFrom :: seq p -> HiddenFrom seq p

commuteOut :: Commute p => RL (PatchInfoAnd p) -> PatchInfoAnd p -> HiddenFrom RL (PatchInfoAnd p)
commuteOut NilRL _ = HiddenFrom NilRL
commuteOut (p :<: ps) me = case commute (p :> me) of
                              Just (me' :> p') ->
                                  case commuteOut ps me' of
                                  HiddenFrom ps' ->
                                      HiddenFrom (p' :<: ps')
                              Nothing ->
                                  case commuteOut ps p of
                                  HiddenFrom ps' ->
                                      commuteOut ps' me
