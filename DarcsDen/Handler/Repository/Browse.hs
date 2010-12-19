{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository.Browse where

import Darcs.Utils (withCurrentDirectory)
import Data.Data (Data)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (isJust, listToMaybe)
import Data.Typeable (Typeable)
import Text.Pandoc
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Repository.InternalTypes as RI
import qualified Data.ByteString.Lazy as LBS
import qualified Storage.Hashed.Tree as T
import qualified Storage.Hashed.AnchoredPath as A

import DarcsDen.Handler.Repository.Util
import DarcsDen.Util


data RepoItem = RepoItem { iName :: String
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

getFiles :: R.Repository P.Patch -> [String] -> IO (Maybe [RepoItem])
getFiles r f = do tree <- repoTree r f
                  return $ fmap (\t -> sort . map (item t . fst) . onelevel $ t) tree
    where onelevel = filter (\(A.AnchoredPath x, _) -> length x == 1) . T.list
          item t a = RepoItem { iIsDirectory = isJust (T.findTree t a)
                              , iPath = "" -- Filled up before sending to template
                              , iName = fromAnchored a
                              }

getBlob :: R.Repository P.Patch -> [String] -> IO (Maybe LBS.ByteString)
getBlob dr@(RI.Repo p _ _ _) f
  = withCurrentDirectory p $ do
       tree <- repoTree dr []
       case tree of
         Nothing -> return Nothing
         Just t -> case T.findFile t (toAnchored f) of
           Nothing -> return Nothing
           Just b -> fmap Just (T.readBlob b)

isTooLarge :: LBS.ByteString -> Bool
isTooLarge = (> (1024 * 1024)) . LBS.length

repoTree :: R.Repository P.Patch -> [String] -> IO (Maybe (T.Tree IO))
repoTree r@(RI.Repo p _ _ _) f
  = do root <- withCurrentDirectory p (R.readRecorded r >>= T.expand)
       if null f
         then return $ Just root
         else return $ T.findTree root (toAnchored f)

getReadme :: R.Repository P.Patch -> [String] -> IO (Maybe String)
getReadme dr f = do
    tree <- repoTree dr f
    case findReadmes tree of
      Nothing -> return Nothing
      Just r -> do
          s <- getBlob dr (f ++ [r])
          case s of
               Nothing -> return Nothing
               Just big | isTooLarge big -> return (Just "README file too large. Sorry.")
               Just md | isMarkdown r ->
                   return . Just . doMarkdown . fromLBS $ md
               Just source ->
                   fmap Just $ highlight False r . fromLBS $ source
    where
        findReadmes = maybe Nothing listToMaybe . fmap (filter isReadme . map (fromAnchored . fst) . T.list)
        doMarkdown = writeHtmlString defaultWriterOptions . readMarkdown defaultParserState
        isReadme s = "README" `isPrefixOf` s || "readme" `isPrefixOf` s
        isMarkdown s = ".markdown" `isSuffixOf` s || ".md" `isSuffixOf` s

toAnchored :: [String] -> A.AnchoredPath
toAnchored = A.AnchoredPath . map A.makeName

fromAnchored :: A.AnchoredPath -> String
fromAnchored = fromBS . A.flatten
