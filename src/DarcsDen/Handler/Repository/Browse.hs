{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository.Browse where

import Darcs.Utils (withCurrentDirectory)
import Data.Char (toLower)
import Data.Data (Data)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (isJust, listToMaybe)
import Data.Typeable (Typeable)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Repository.InternalTypes as RI
import qualified Data.ByteString.Lazy as LBS
import qualified Storage.Hashed.Tree as T
import qualified Storage.Hashed.AnchoredPath as A

import DarcsDen.Handler.Repository.Util
import DarcsDen.Util


data RepoItem =
    RepoItem
        { iName :: String
        , iPath :: String
        , iIsDirectory :: Bool
        }
    deriving (Eq, Show, Data, Typeable)

instance Ord RepoItem where
  compare (RepoItem _ _ True) (RepoItem _ _ False) = LT
  compare (RepoItem _ _ False) (RepoItem _ _ True) = GT
  compare (RepoItem a _ _) (RepoItem b _ _) = compare a b


pathToFile :: [String] -> String
pathToFile [] = ""
pathToFile f = '/' : intercalate "/" f

getFiles :: P.RepoPatch p => R.Repository p -> [String] -> IO (Maybe [RepoItem])
getFiles r f = do
    tree <- repoTree r f
    return $ fmap (\t -> sort . map (item t . fst) . onelevel $ t) tree
  where
    onelevel = filter (\(A.AnchoredPath x, _) -> length x == 1) . T.list
    item t a = RepoItem
        { iIsDirectory = isJust (T.findTree t a)
        , iPath = "" -- Filled up before sending to template
        , iName = fromAnchored a
        }

getBlob :: P.RepoPatch p => R.Repository p -> [String] -> IO (Maybe LBS.ByteString)
getBlob dr@(RI.Repo p _ _ _) f = withCurrentDirectory p $ do
    tree <- repoTree dr []
    maybe (return Nothing) (\t ->
        case T.findFile t (toAnchored f) of
            Nothing -> return Nothing
            Just b -> fmap Just (T.readBlob b)) tree

isTooLarge :: LBS.ByteString -> Bool
isTooLarge = (> (1024 * 1024)) . LBS.length

repoTree :: P.RepoPatch p => R.Repository p -> [String] -> IO (Maybe (T.Tree IO))
repoTree r@(RI.Repo p _ _ _) f = do
    root <- withCurrentDirectory p (R.readRecorded r >>= T.expand)
    return $
        if null f
            then Just root
            else T.findTree root (toAnchored f)

getReadme :: P.RepoPatch p => R.Repository p -> [String] -> IO (Maybe String)
getReadme dr f = do
    tree <- repoTree dr f
    case findReadmes tree of
        Nothing -> return Nothing
        Just r -> do
            s <- getBlob dr (f ++ [r])
            return $
                case s of
                    Nothing -> Nothing
                    Just big | isTooLarge big ->
                        Just "README file too large. Sorry."
                    Just md | isMarkdown r ->
                        Just . doMarkdown . fromLBS $ md
                    Just source ->
                        Just . fromBS . highlight False r . strictLBS $ source
  where
    findReadmes
        = maybe Nothing listToMaybe
        . fmap (filter isReadme . map (fromAnchored . fst) . T.list)

    isReadme s = "readme" `isPrefixOf` (map toLower s)
    isMarkdown s = or
        [ ".markdown" `isSuffixOf` (map toLower s)
        , ".md" `isSuffixOf` (map toLower s)
        ]

toAnchored :: [String] -> A.AnchoredPath
toAnchored = A.AnchoredPath . map A.makeName

fromAnchored :: A.AnchoredPath -> String
fromAnchored = fromBS . A.flatten
