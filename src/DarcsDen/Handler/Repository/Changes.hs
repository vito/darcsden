{-# LANGUAGE GADTs #-}
module DarcsDen.Handler.Repository.Changes where

import Control.Concurrent
import Control.Monad
import Darcs.Patch.Info (pi_date, pi_name, pi_author, pi_log, make_filename)
import Darcs.Patch.FileName (fn2fp)
import Darcs.Patch.Patchy (Commute(..))
import Darcs.Patch.Prim (Prim(..), DirPatchType(..), FilePatchType(..))
import Darcs.Hopefully (PatchInfoAnd, info)
import Darcs.Witnesses.Ordered
import Data.List (nub)
import Data.Time (UTCTime, readTime)
import System.Locale (defaultTimeLocale)
import System.Time (calendarTimeToString)
import Text.Pandoc
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Witnesses.Ordered as WO
import qualified Data.HashTable as HT

import DarcsDen.Handler.Repository.Util
import DarcsDen.State.User
import DarcsDen.Util


data Summary
    = Removed FilePath
    | Added FilePath
    | Replaced FilePath String String
    | Modified FilePath
    | Preference String String String
    deriving (Eq, Show)

data PatchLog =
    PatchLog
        { pID :: String
        , pDate :: UTCTime
        , pName :: String
        , pAuthor :: String
        , pIsUser :: Bool
        , pLog :: String
        , pDepends :: [String]
        }
    deriving (Eq, Show)


data PatchChange
    = Moved
        { cmFrom :: FilePath
        , cmTo :: FilePath
        }
    | DirChange
        { cdName :: FilePath
        , cdType :: DirChange
        }
    | FileChange
        { cfName :: FilePath
        , cfType :: FileChange
        }
    | PrefChange
        { cpName :: String
        , cpFrom :: String
        , cpTo :: String
        }
    deriving (Eq, Show)

data DirChange
    = DirRemoved
    | DirAdded
    deriving (Eq, Show)

data FileChange
    = FileRemoved
    | FileAdded
    | FileHunk
        { fchLine :: Int
        , fchRemove :: String
        , fchAdd :: String
        }
    | FileBinary
    | FileReplace
        { fchFind :: String
        , fchReplace :: String
        }
    deriving (Eq, Show)

data PatchChanges =
    PatchChanges
        { pPatch :: PatchLog
        , pChanges :: [PatchChange]
        }
    deriving (Eq, Show)


toLog :: P.Named p -> PatchLog
toLog p =
    PatchLog
        (take 20 $ make_filename i)
        (readTime defaultTimeLocale "%c" (calendarTimeToString $ pi_date i))
        (pi_name i)
        (pi_author i)
        False
        (doMarkdown . unlines $ pi_log i)
        (map (take 20 . make_filename) (P.getdeps p))
  where
    i = P.patch2patchinfo p
    doMarkdown
        = writeHtmlString defaultWriterOptions
        . readMarkdown defaultParserState

findUsers :: [PatchLog] -> IO [PatchLog]
findUsers = findUsers' []
  where
    emailFrom = reverse . takeWhile (/= '<') . tail . reverse
    authorFrom a = let name = takeWhile (/= '<') a
                   in if last name == ' '
                         then init name
                         else name

    findUsers' :: [(String, Maybe String)] -> [PatchLog] -> IO [PatchLog]
    findUsers' _ [] = return []
    findUsers' checked (p:ps) =
        case lookup (pAuthor p) checked of
            Just (Just n) -> do
                rest <- findUsers' checked ps
                return (p { pAuthor = n, pIsUser = True } : rest)
            Just Nothing -> do
                rest <- findUsers' checked ps
                return (p { pAuthor = authorFrom (pAuthor p) } : rest)
            Nothing -> do
                mu <- getUserByEmail (emailFrom (pAuthor p))
                case mu of
                    Just u -> do
                        rest <- findUsers' ((pAuthor p, Just (uName u)) : checked) ps
                        return (p { pAuthor = uName u, pIsUser = True } : rest)
                    Nothing -> do
                        rest <- findUsers' ((pAuthor p, Nothing) : checked) ps
                        return (p { pAuthor = authorFrom (pAuthor p) } : rest)

toChanges :: P.Effect p => P.Named p -> IO PatchChanges
toChanges p = do
    wait <- newChan
    cs <- HT.new (==) fromIntegral :: IO (HT.HashTable Int PatchChange)

    count <- simplify [] changes cs wait

    replicateM_ count (readChan wait)

    fmap (PatchChanges (toLog p) . map snd) (HT.toList cs)
  where
    changes = map primToChange (WO.unsafeUnFL (P.effect p))

    simplify a [] = \cs wait -> do
        forM_ (zip a [0..]) $ \(c, n) ->
            case c of
                FileChange fn (FileHunk l f t) -> do
                    forkIO $ do
                        hf <- highlight False fn f
                        ht <- highlight False fn t
                        HT.insert cs n (FileChange fn (FileHunk l hf ht))
                        writeChan wait n

                    return ()

                _ -> do
                    HT.insert cs n c
                    writeChan wait n

        return (length a)
    simplify a (c@(FileChange n t):cs)
        | t `elem` [FileAdded, FileRemoved, FileBinary] =
            simplify (c:filter (notFile n) a) (filter (notFile n) cs)
    simplify a (c@(FileChange _ (FileReplace _ _)):cs) =
        simplify (c:a) cs
    simplify a (c@(FileChange _ _):cs) = simplify (c:a) cs
    simplify a (c@(PrefChange _ _ _):cs) = simplify (c:a) cs
    simplify a (_:cs) = simplify a cs

    notFile n (FileChange { cfName = n' })
        | n == n' = False
    notFile _ _ = True

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
getChanges dir page = R.withRepositoryDirectory [] dir $ \dr -> do
    pset <- R.read_repo dr

    let ps = fromPS pset
        patches = map toLog (paginate 30 page ps)

    prettyLog <- findUsers patches

    return
        ( prettyLog
        , ceiling ((fromIntegral (length ps) :: Double) / 30)
        )

getPatch :: String -> String -> IO PatchChanges
getPatch dir patch = R.withRepositoryDirectory [] dir $ \dr -> do
    pset <- R.read_repo dr

    let ps = fromPS pset
        p = head $ filter (\p' -> patch == take 20 (P.patchname p')) ps

    cs <- toChanges p

    [l] <- findUsers [pPatch cs]
    return cs { pPatch = l }

fromPS :: P.RepoPatch p => R.PatchSet p -> [P.Named p]
fromPS = WO.unsafeUnRL . WO.reverseFL . R.patchSetToPatches

summarize :: [PatchChange] -> [Summary]
summarize = nub . reverse . summarize'
  where
    summarize' [] = []
    summarize' (FileChange n FileRemoved:cs) = (Removed n) : summarize cs
    summarize' (FileChange n FileAdded:cs) = (Added n) : summarize cs
    summarize' (FileChange n (FileReplace f t):cs) = (Replaced n f t) : summarize cs
    summarize' (FileChange n _:cs) = (Modified n) : summarize cs
    summarize' (PrefChange n f t:cs) = (Preference n f t) : summarize cs
    summarize' (_:cs) = summarize cs

isModification :: PatchChange -> Bool
isModification (FileChange _ (FileHunk _ _ _)) = True
isModification _ = False

-- The following is ported over from Camp.
findAllDeps :: Commute p => FL (PatchInfoAnd p) -> [(String, [PatchInfoAnd p])]
findAllDeps = f NilRL
  where
    f :: Commute p => RL (PatchInfoAnd p) -> FL (PatchInfoAnd p) -> [(String, [PatchInfoAnd p])]
    f _ NilFL = []
    f past (p :>: ps) =
        (make_filename (info p), findDeps past p) : f (p :<: past) ps

findDeps :: Commute p => RL (PatchInfoAnd p) -> PatchInfoAnd p -> [PatchInfoAnd p]
findDeps NilRL _ = []
findDeps (p :<: ps) me =
    case commute (p :> me) of
        Just (me' :> _) -> findDeps ps me'
        Nothing ->
            case commuteOut ps p of
                HiddenFrom ps' ->
                    p : findDeps ps' me

data HiddenFrom seq p
    where HiddenFrom :: seq p -> HiddenFrom seq p

commuteOut :: Commute p => RL (PatchInfoAnd p) -> PatchInfoAnd p -> HiddenFrom RL (PatchInfoAnd p)
commuteOut NilRL _ = HiddenFrom NilRL
commuteOut (p :<: ps) me =
    case commute (p :> me) of
        Just (me' :> p') ->
            case commuteOut ps me' of
                HiddenFrom ps' ->
                    HiddenFrom (p' :<: ps')
        Nothing ->
           case commuteOut ps p of
               HiddenFrom ps' ->
                   commuteOut ps' me
