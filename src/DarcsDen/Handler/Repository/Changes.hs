module DarcsDen.Handler.Repository.Changes where

import Control.Concurrent
import Control.Monad
import Darcs.Patch.Info (PatchInfo(..), piDate, makeFilename)
import Darcs.Patch.FileName (fn2fp)
import Darcs.Patch.PatchInfoAnd (PatchInfoAnd, info)
import Darcs.Patch.Patchy (Commute(..))
import Darcs.Patch.Permutations (commuteWhatWeCanFL)
import Darcs.Patch.Prim (Prim(..), DirPatchType(..), FilePatchType(..))
import Darcs.Witnesses.Ordered
import Data.List (isPrefixOf, nub)
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


toLog :: (PatchInfo, [PatchInfo]) -> PatchLog
toLog (i, ds) =
    PatchLog
        (take 20 $ makeFilename i)
        (readTime defaultTimeLocale "%c" (calendarTimeToString (piDate i)))
        (fromBS $ _piName i)
        (fromBS $ _piAuthor i)
        False
        (doMarkdown . unlines . filter (not . ("Ignore-this" `isPrefixOf`)) . map fromBS $ _piLog i)
        (map (take 20 . makeFilename) ds)
  where
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

toChanges :: ((PatchInfo, [PatchInfo]), [PatchChange])  -> IO PatchChanges
toChanges (p, changes) = do
    wait <- newChan
    cs <- HT.new (==) fromIntegral :: IO (HT.HashTable Int PatchChange)

    count <- simplify [] changes cs wait

    replicateM_ count (readChan wait)

    fmap (PatchChanges (toLog p) . map snd) (HT.toList cs)
  where
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

primToChange :: Prim x y -> PatchChange
primToChange (Move f t) = Moved (drop 2 $ fn2fp f) (drop 2 $ fn2fp t)
primToChange (DP f t) = DirChange (drop 2 $ fn2fp f) (fromDP t)
primToChange (FP f t) = FileChange (drop 2 $ fn2fp f) (fromFP t)
primToChange (ChangePref n f t) = PrefChange n f t
primToChange a = error ("primToChange not supported for " ++ show a)

fromDP :: DirPatchType x y -> DirChange
fromDP RmDir = DirRemoved
fromDP AddDir = DirAdded

fromFP :: FilePatchType x y -> FileChange
fromFP RmFile = FileRemoved
fromFP AddFile = FileAdded
fromFP (Hunk l rs as) = FileHunk l (unlines $ map fromBS rs) (unlines $ map fromBS as)
fromFP (Binary _ _) = FileBinary
fromFP (TokReplace _ f r) = FileReplace f r

getChanges :: String -> Int -> IO ([PatchLog], Int)
getChanges dir page = R.withRepositoryDirectory [] dir $ \dr -> do
    pset <- R.readRepo dr

    let ps = fromPS (\np -> (P.patch2patchinfo np, P.getdeps np)) pset
        patches = map toLog (paginate 30 page ps)

    prettyLog <- findUsers patches

    return
        ( prettyLog
        , ceiling ((fromIntegral (length ps) :: Double) / 30)
        )

getPatch :: String -> String -> IO PatchChanges
getPatch dir patch = R.withRepositoryDirectory [] dir $ \dr -> do
    pset <- R.readRepo dr

    let ps = fromPS (\np -> (P.patchname np, ((P.patch2patchinfo np, P.getdeps np), WO.mapFL primToChange (P.effect np)))) pset
        (_, p) = head $ filter (\(n, _) -> patch == take 20 n) ps

    cs <- toChanges p

    [l] <- findUsers [pPatch cs]
    return cs { pPatch = l }

fromPS :: P.RepoPatch p => (forall w z. P.Named p w z -> b) -> R.PatchSet p x y -> [b]
fromPS f = WO.mapRL f . WO.reverseFL . R.patchSetToPatches

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

findAllDeps :: Commute p => RL (PatchInfoAnd p) wX wY -> [(PatchInfo, [PatchInfo])]
findAllDeps NilRL = []
findAllDeps (p :<: ps) = (info p, findDeps ps NilFL p) : findAllDeps ps

findDeps :: Commute p => RL (PatchInfoAnd p) wX wY -> FL (PatchInfoAnd p) wY wZ -> PatchInfoAnd p wZ wT -> [PatchInfo]
findDeps NilRL _ _ = []
findDeps (p :<: ps) deps me =
    case commuteWhatWeCanFL (p :> deps) of
        deps' :> p' :> NilFL ->
            case commute (p' :> me) of
                Just (me' :> _) ->
                    -- not a dependency
                    findDeps ps deps' me'

                Nothing ->
                    -- a direct dependency
                    info p' : findDeps ps (p :>: deps) me
        _ ->
            -- an indirect dependency
            findDeps ps (p :>: deps) me