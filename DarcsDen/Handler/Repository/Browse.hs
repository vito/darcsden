{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository.Browse where

import Darcs.Utils (withCurrentDirectory)
import Data.Data (Data)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Text.Pandoc
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Repository.InternalTypes as RI
import qualified Storage.Hashed.Tree as T
import qualified Storage.Hashed.AnchoredPath as A

import DarcsDen.Handler.Repository.Util
import DarcsDen.Util


data RepoItem = RepoItem { iName :: String
                         , iURL :: String
                         , iIsDirectory :: Bool
                         }
                deriving (Eq, Show, Data, Typeable)

instance Ord RepoItem where
  compare (RepoItem _ _ True) (RepoItem _ _ False) = LT
  compare (RepoItem _ _ False) (RepoItem _ _ True) = GT
  compare (RepoItem a _ _) (RepoItem b _ _) = compare a b



urlTo :: String -> String -> [String] -> String
urlTo un rn f = "/" ++ un ++ "/" ++ rn ++ "/browse" ++ (if null f then "" else '/' : intercalate "/" f)

getFiles :: R.Repository P.Patch -> [String] -> IO (Maybe [RepoItem])
getFiles r f = do tree <- repoTree r f
                  return $ fmap (\t -> sort . map (item t . fst) . onelevel $ t) tree
    where onelevel = filter (\(A.AnchoredPath x, _) -> length x == 1) . T.list
          item t a = RepoItem { iIsDirectory = isJust (T.findTree t a)
                              , iURL = "" -- Filled up there
                              , iName = fromAnchored a
                              }

getBlob :: R.Repository P.Patch -> [String] -> IO (Maybe String)
getBlob dr@(RI.Repo p _ _ _) f
  = withCurrentDirectory p $ do
       tree <- repoTree dr []
       case tree of
         Nothing -> return Nothing
         Just t -> case T.findFile t (toAnchored f) of
           Nothing -> return Nothing
           Just b -> fmap (Just . fromLBS) (T.readBlob b)

repoTree :: R.Repository P.Patch -> [String] -> IO (Maybe (T.Tree IO))
repoTree r@(RI.Repo p _ _ _) f
  = do root <- withCurrentDirectory p (R.readRecorded r >>= T.expand)
       if null f
         then return $ Just root
         else return $ T.findTree root (toAnchored f)

getReadme :: R.Repository P.Patch -> [String] -> IO (Maybe String)
getReadme dr f = do tree <- repoTree dr f
                    case tree of
                      Nothing -> return Nothing
                      Just t -> let readmes = map (fromAnchored . fst) $ filter (\(a, _) -> "README" `isPrefixOf` fromAnchored a) (T.list t)
                                in case readmes of
                                  [] -> return Nothing
                                  (r:_) -> do s <- getBlob dr (f ++ [r])
                                              if ".markdown" `isSuffixOf` r || ".md" `isSuffixOf` r
                                                then return $ fmap (writeHtmlString defaultWriterOptions . readMarkdown defaultParserState) s
                                                else return $ fmap (flip (highlight r) []) s

toAnchored :: [String] -> A.AnchoredPath
toAnchored = A.AnchoredPath . map A.makeName

fromAnchored :: A.AnchoredPath -> String
fromAnchored = fromBS . A.flatten
