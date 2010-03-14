{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository.Changes where

import Darcs.Patch.Info (PatchInfo, pi_date, pi_name, pi_author, pi_log, make_filename)
import Darcs.Patch.FileName (fn2fp)
import Darcs.Patch.Prim (Prim(..), DirPatchType(..), FilePatchType(..))
import Data.List (nub)
import Happstack.State (query)
import System.Time (calendarTimeToString)
import Text.JSON.Generic
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Witnesses.Ordered as WO

import DarcsDen.Handler.Repository.Util
import DarcsDen.State.User


data PatchLog = PatchLog { pID :: String
                         , pDate :: String
                         , pName :: String
                         , pAuthor :: Either String User
                         , pLog :: [String]
                         }
                deriving (Eq, Show, Data, Typeable)


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
                 deriving (Eq, Show, Data, Typeable)

data DirChange = DirRemoved | DirAdded
               deriving (Eq, Show, Data, Typeable)

data FileChange = FileRemoved
                | FileAdded
                | FileHunk { fchLine :: Int
                           , fchRemove :: String
                           , fchAdd :: String
                           }
                | FileBinary
                deriving (Eq, Show, Data, Typeable)

data PatchChanges = PatchChanges { pPatch :: PatchLog
                                 , pChanges :: [PatchChange]
                                 }
                    deriving (Eq, Show, Data, Typeable)


toLog :: PatchInfo -> IO PatchLog
toLog p = do mu <- query $ GetUserByEmail (emailFrom (pi_author p))

             let author = case mu of
                   Nothing -> Left (pi_author p)
                   Just u -> Right u

             return $ PatchLog (take 20 $ make_filename p) (calendarTimeToString $ pi_date p) (pi_name p) author (pi_log p)
  where emailFrom = reverse . takeWhile (/= '<') . tail . reverse

toChanges :: P.Effect p => P.Named p -> IO PatchChanges
toChanges p = do l <- toLog (P.patch2patchinfo p)
                 return . PatchChanges l . simplify [] . map primToChange . WO.unsafeUnFL $ P.effect p
  where simplify a [] = reverse a
        simplify a (c@(FileChange n t):cs) | t `elem` [FileAdded, FileRemoved, FileBinary]
          = simplify (c:filter (notFile n) a) (filter (notFile n) cs)
        simplify a ((FileChange n (FileHunk l f t)):cs)
          = simplify ((FileChange n (FileHunk l (hl n f) (hl n t))):a) cs
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
fromFP a = error ("fromFP not supported for " ++ show a)

getChanges :: String -> Int -> IO ([PatchLog], Int)
getChanges dir  page = R.withRepositoryDirectory [] dir $ \dr ->
  do pset <- R.read_repo dr
     let ps = fromPS pset
     ls <- mapM (toLog . P.patch2patchinfo) . paginate $ ps
     return (ls, ceiling ((fromIntegral (length ps) :: Double) / 30))
  where paginate = take 30 . drop (30 * (page - 1))

getPatch :: String -> String -> IO PatchChanges
getPatch dir patch = R.withRepositoryDirectory [] dir $ \dr ->
  do pset <- R.read_repo dr
     let ps = fromPS pset
         p = head $ filter (\p' -> patch == take 20 (P.patchname p')) ps

     toChanges p

fromPS :: P.RepoPatch p => R.PatchSet p -> [P.Named p]
fromPS = WO.unsafeUnRL . WO.reverseFL . R.patchSetToPatches

summarize :: [[(String, JSValue)]] -> [PatchChange] -> [JSValue]
summarize a [] = map (JSObject . toJSObject) (nub a)
summarize a ((FileChange n FileRemoved):cs) = summarize (a ++ [[("removed", toJSON n)]]) cs
summarize a ((FileChange n FileAdded):cs) = summarize (a ++ [[("added", toJSON n)]]) cs
summarize a ((FileChange n _):cs) = summarize (a ++ [[("modified", toJSON n)]]) cs
summarize a ((PrefChange n f t):cs) = summarize (a ++ [[ ("preference", toJSON n)
                                                       , ("from", toJSON f)
                                                       , ("to", toJSON t)
                                                       , ("type", toJSON "change")
                                                       ]]) cs
summarize a (_:cs) = summarize a cs

isModification :: PatchChange -> Bool
isModification (FileChange _ (FileHunk _ _ _)) = True
isModification _ = False