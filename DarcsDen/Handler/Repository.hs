{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module DarcsDen.Handler.Repository where

import Data.Char (chr)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List (inits, intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Map ((!))
import Hack
import Happstack.State
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import System.Time (getClockTime, CalendarTime)
import Text.Highlighting.Kate
import Text.XHtml.Strict (renderHtmlFragment)
import Text.Pandoc
import qualified Darcs.Patch as P
import Darcs.Patch.Info (PatchInfo, pi_date, pi_name, pi_author, pi_log)
import qualified Darcs.Hopefully as H
import qualified Darcs.Repository as R
import qualified Darcs.Repository.InternalTypes as RI
import qualified Darcs.Witnesses.Ordered as RL
import qualified Storage.Hashed.AnchoredPath as A
import qualified Storage.Hashed.Tree as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS

import DarcsDen.Data
import DarcsDen.HackUtils
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Validate
import DarcsDen.DarcsChanges


data RepoItem = RepoItem { iName :: String
                         , iURL :: String
                         , iIsDirectory :: Bool
                         }
                deriving (Eq, Show, Data, Typeable)

data PatchLog = PatchLog { pDate :: CalendarTime
                         , pName :: String
                         , pAuthor :: String
                         , pLog :: [String]
                         }
                deriving (Show, Data, Typeable)


instance Ord RepoItem where
  compare (RepoItem _ _ True) (RepoItem _ _ False) = LT
  compare (RepoItem _ _ False) (RepoItem _ _ True) = GT
  compare (RepoItem a _ _) (RepoItem b _ _) = compare a b

initialize :: Page
initialize (Session { sUser = Nothing }) _ = redirectTo "/"
initialize _ e@(Env { requestMethod = GET }) = doPage "init" [] e
initialize (Session { sUser = Just n }) e
  = validate e
    [ nonEmpty "name"
    , io "user is not valid" (query (GetUser n) >>= (return . (/= Nothing)))
    ]
    (\(OK r) -> do now <- getClockTime
                   newRepository $ Repository { rName = r ! "name"
                                              , rDescription = input "description" "" e
                                              , rWebsite = input "website" "" e
                                              , rOwner = n
                                              , rUsers = []
                                              , rCreated = now
                                              }
                   redirectTo "/")
    (\(Invalid f) -> doPage "init" [var "failed" (map explain f), assocObj "in" (getInputs e)] e)

repository :: String -> String -> Page
repository un rn s e = browseRepository un rn [] s e

browseRepository :: String -> String -> [String] -> Page
browseRepository un rn f s e
  = validate e [ io "user does not exist" $ query (GetUser un) >>= return . (/= Nothing)
               , io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing)
               , io "repository invalid" $ do
                   mr <- R.maybeIdentifyRepository [] ("repos/" ++ un ++ "/" ++ rn)
                   case mr of
                     Left _ -> return False
                     Right _ -> return True
               ]
    (\(OK _) -> do
      Just u <- query (GetUser un)
      Just r <- query (GetRepository (un, rn))
      Right dr <- R.maybeIdentifyRepository [] ("repos/" ++ un ++ "/" ++ rn)

      fs <- files dr f
      bl <- blob dr f

      let path = (map (\p -> RepoItem { iName = last p
                                      , iURL = urlTo un rn p
                                      , iIsDirectory = True
                                      }) (tail $ inits f))

      case (fs, bl) of
        (Nothing, Nothing) -> notFound s e
        (Just fs', _) -> do
          readme <- getReadme dr f
          doPage "repo" [ var "user" u
                        , var "repo" r
                        , var "files" (map (\i -> i { iURL = urlTo un rn (f ++ [iName i]) }) fs')
                        , var "up" (if null f
                                      then ""
                                      else urlTo un rn (init f))
                        , var "path" path
                        , var "readme" (maybe "" id readme)
                        ] e
        (_, Just source) ->
          doPage "repo-blob" [ var "user" u
                             , var "repo" r
                             , var "file" (last path)
                             , var "blob" (highlight (last f) source [OptNumberLines])
                             , var "path" (init path)
                             ] e)
    (\(Invalid _) -> notFound s e)


repositoryLog :: String -> String -> Page
repositoryLog un rn s e
  = validate e [ io "user does not exist" $ query (GetUser un) >>= return . (/= Nothing)
               , io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing)
               , io "repository invalid" $ do
                   mr <- R.maybeIdentifyRepository [] ("repos/" ++ un ++ "/" ++ rn)
                   case mr of
                     Left _ -> return False
                     Right _ -> return True
               ]
    (\(OK _) -> do
      Just u <- query (GetUser un)
      Just r <- query (GetRepository (un, rn))
      Right dr <- getRepo ("repos/" ++ un ++ "/" ++ rn)

      read <- R.read_repo dr
      let (ps, _, _) = getChangesInfo [] [] read
          patches = map (\(i, _) -> toLog . H.info $ i) ps

      doPage "repo-log" [var "patches" patches] e)
    (\(Invalid _) -> notFound s e)
    where toLog p = PatchLog (pi_date p) (pi_name p) (pi_author p) (pi_log p)

files :: R.Repository P.Patch -> [String] -> IO (Maybe [RepoItem])
files r f = do tree <- repoTree r f
               return $ fmap (\t -> sort . map (item t . fst) . onelevel $ t) tree
    where onelevel = filter (\(A.AnchoredPath x, _) -> length x == 1) . T.list
          item t a = RepoItem { iIsDirectory = (maybe False (const True) (T.findTree t a))
                              , iURL = "" -- Filled up there
                              , iName = (fromBS . A.flatten $ a)
                              }

getRepo :: String -> IO (Either String (R.Repository P.Patch))
getRepo = R.maybeIdentifyRepository []

repoTree :: R.Repository P.Patch -> [String] -> IO (Maybe (T.Tree IO))
repoTree r@(RI.Repo p _ _ _) f
  = do root <- withDirectory p (R.readRecorded r >>= T.expand)
       if null f
         then return $ Just root
         else return $ T.findTree root (toAnchored f)

blob :: R.Repository P.Patch -> [String] -> IO (Maybe String)
blob (RI.Repo p _ _ _) f
  = let filename = fromAnchored $ A.AnchoredPath (map A.makeName f)
    in withDirectory p $ do
      c <- doesFileExist filename
      if c
        then readFile filename >>= return . Just
        else return Nothing
-- FIXME: This would be better but readBlob it ends up with a Zlib error from reading the blob.
-- blob dr f
  -- = do tree <- repoTree dr []
  --      case tree of
  --        Nothing -> return Nothing
  --        Just t -> case T.findFile t (toAnchored f) of
  --          Nothing -> return Nothing
  --          Just b@(T.Blob _ h) -> do putStrLn "grabbing blob..."
  --                                    print h
  --                                    b' <- T.readBlob b
  --                                    putStrLn "grabbed."
  --                                    print (LS.length b')
  --                                    return . Just . fromLS $ b'

highlight :: String -> String -> [FormatOption] -> String
highlight f s os = case hl of
                     Right res -> renderHtmlFragment (formatAsXHtml os lang res)
                     Left _ -> s
    where langs = languagesByExtension (takeExtension f)
          lang = if null langs then "text" else (head langs)
          hl = if null langs
                 then Right (map (\l -> [([], l)]) (lines s))
                 else highlightAs lang s

fromBS :: BS.ByteString -> String
fromBS = map (chr . fromIntegral) . BS.unpack

fromLS :: LS.ByteString -> String
fromLS = map (chr . fromIntegral) . LS.unpack

urlTo :: String -> String -> [String] -> String
urlTo un rn f = "/" ++ un ++ "/" ++ rn ++ "/browse" ++ (if null f then "" else "/" ++ intercalate "/" f)

getReadme :: R.Repository P.Patch -> [String] -> IO (Maybe String)
getReadme dr f = do tree <- repoTree dr f
                    case tree of
                      Nothing -> return Nothing
                      Just t -> let readmes = map (fromAnchored . fst) $ filter (\(a, _) -> "README" `isPrefixOf` (fromAnchored a)) (T.list t)
                                in case readmes of
                                  [] -> return Nothing
                                  (r:_) -> do s <- blob dr (f ++ [r])
                                              if ".markdown" `isSuffixOf` r || ".md" `isSuffixOf` r
                                                then return $ fmap (writeHtmlString defaultWriterOptions . readMarkdown defaultParserState) s
                                                else return $ fmap (flip (highlight r) []) s


toAnchored :: [String] -> A.AnchoredPath
toAnchored = A.AnchoredPath . map A.makeName

fromAnchored :: A.AnchoredPath -> String
fromAnchored = fromBS . A.flatten
