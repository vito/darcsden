{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module DarcsDen.Handler.Repository where

import Darcs.Patch.FileName (fn2fp)
import Darcs.Patch.Info (PatchInfo, pi_date, pi_name, pi_author, pi_log, make_filename)
import Darcs.Patch.Prim (Prim(..), DirPatchType(..), FilePatchType(..))
import Darcs.Utils (withCurrentDirectory)
import Data.Char (chr)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List (inits, intercalate, isPrefixOf, isSuffixOf, nub, sort)
import Data.Map ((!))
import Hack
import Happstack.State
import System.FilePath (takeExtension)
import System.Time (getClockTime, CalendarTime, calendarTimeToString)
import Text.JSON.Generic
import Text.Highlighting.Kate
import Text.XHtml.Strict (renderHtmlFragment)
import Text.Pandoc
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Repository.InternalTypes as RI
import qualified Darcs.Witnesses.Ordered as WO
import qualified Storage.Hashed.AnchoredPath as A
import qualified Storage.Hashed.Tree as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS

import DarcsDen.Data ()
import DarcsDen.HackUtils
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Validate


data RepoItem = RepoItem { iName :: String
                         , iURL :: String
                         , iIsDirectory :: Bool
                         }
                deriving (Eq, Show, Data, Typeable)

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


instance Ord RepoItem where
  compare (RepoItem _ _ True) (RepoItem _ _ False) = LT
  compare (RepoItem _ _ False) (RepoItem _ _ True) = GT
  compare (RepoItem a _ _) (RepoItem b _ _) = compare a b

initialize :: Page
initialize s@(Session { sUser = Nothing }) _ = warn "You must be logged in to create a repository." s >> redirectTo "/"
initialize s e@(Env { requestMethod = GET }) = doPage "init" [] s e
initialize s@(Session { sUser = Just n }) e
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
    (\(Invalid f) -> doPage "init" [var "failed" (map explain f), assocObj "in" (getInputs e)] s e)

repository :: String -> String -> Page
repository un rn s e = browseRepository un rn [] s e

browseRepository :: String -> String -> [String] -> Page
browseRepository un rn f s e
  = validate e [ io "user does not exist" $ query (GetUser un) >>= return . (/= Nothing)
               , io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing)
               , io "repository invalid" $ do
                   mr <- R.maybeIdentifyRepository [] (repoDir un rn)
                   case mr of
                     Left _ -> return False
                     Right _ -> return True
               ]
    (\(OK _) -> do
      Just u <- query (GetUser un)
      Just r <- query (GetRepository (un, rn))
      Right dr <- R.maybeIdentifyRepository [] (repoDir un rn)

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
                        ] s e
        (_, Just source) ->
          doPage "repo-blob" [ var "user" u
                             , var "repo" r
                             , var "file" (last path)
                             , var "blob" (highlight (last f) source [OptNumberLines])
                             , var "path" (init path)
                             ] s e)
    (\(Invalid _) -> notFound s e)


repositoryChanges :: String -> String -> Page
repositoryChanges un rn s e
  = validate e [ io "user does not exist" $ query (GetUser un) >>= return . (/= Nothing)
               , io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing)
               , io "repository invalid" $ do
                   mr <- R.maybeIdentifyRepository [] (repoDir un rn)
                   case mr of
                     Left _ -> return False
                     Right _ -> return True
               ]
    (\(OK _) -> do
      Just u <- query (GetUser un)
      Just r <- query (GetRepository (un, rn))

      patches <- R.withRepositoryDirectory [] (repoDir un rn) $ \dr -> do
        ps <- R.read_repo dr
        sequence $ WO.mapRL (\p -> toLog (P.patch2patchinfo p)) $ WO.reverseFL $ R.patchSetToPatches ps

      doPage "repo-changes" [ var "user" u
                            , var "repo" r
                            , var "patches" patches
                            ] s e)
    (\(Invalid _) -> notFound s e)

repositoryPatch :: String -> String -> String -> Page
repositoryPatch un rn p s e
  = validate e [ io "user does not exist" $ query (GetUser un) >>= return . (/= Nothing)
               , io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing)
               , io "repository invalid" $ do
                   mr <- R.maybeIdentifyRepository [] (repoDir un rn)
                   case mr of
                     Left _ -> return False
                     Right _ -> return True
               ]
    (\(OK _) -> do
      Just u <- query (GetUser un)
      Just r <- query (GetRepository (un, rn))

      patch <- R.withRepositoryDirectory [] (repoDir un rn) (\dr -> do
        ps <- R.read_repo dr

        let ps' = R.patchSetToPatches ps
            patch = head $ filter (\p' -> p `isPrefixOf` P.patchname p') (WO.unsafeUnFL ps')

        toChanges patch)

      doPage "repo-patch" [ var "user" u
                          , var "repo" r
                          , var "log" (pPatch patch)
                          , JSObject $ toJSObject [("summary", JSArray (summarize [] (pChanges patch)))]
                          , var "changes" (filter modification (pChanges patch))
                          ] s e)
    (\(Invalid f) -> print f >> notFound s e)
    where summarize :: [[(String, JSValue)]] -> [PatchChange] -> [JSValue]
          summarize a [] = map (JSObject . toJSObject) (nub a)
          summarize a ((FileChange n FileRemoved):cs) = summarize (a ++ [[("file", toJSON n), ("type", toJSON "removed")]]) cs
          summarize a ((FileChange n FileAdded):cs) = summarize (a ++ [[("file", toJSON n), ("type", toJSON "added")]]) cs
          summarize a ((FileChange n _):cs) = summarize (a ++ [[("file", toJSON n), ("type", toJSON "modified")]]) cs
          summarize a (_:cs) = summarize a cs

          modification (FileChange _ (FileHunk _ _ _)) = True
          modification _ = False

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
  = do root <- withCurrentDirectory p (R.readRecorded r >>= T.expand)
       if null f
         then return $ Just root
         else return $ T.findTree root (toAnchored f)

blob :: R.Repository P.Patch -> [String] -> IO (Maybe String)
blob dr@(RI.Repo p _ _ _) f
  = withCurrentDirectory p $ do
       tree <- repoTree dr []
       case tree of
         Nothing -> return Nothing
         Just t -> case T.findFile t (toAnchored f) of
           Nothing -> return Nothing
           Just b -> T.readBlob b >>= return . Just . fromLS

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

toLog :: PatchInfo -> IO PatchLog
toLog p = do u <- query $ GetUserByEmail (emailFrom (pi_author p))

             let author = case u of
                   Nothing -> Left (pi_author p)
                   Just u -> Right u

             return $ PatchLog (make_filename p) (calendarTimeToString $ pi_date p) (pi_name p) author (pi_log p)
  where emailFrom = reverse . takeWhile (/= '<') . tail . reverse

toChanges :: P.Effect p => P.Named p -> IO PatchChanges
toChanges p = do log <- toLog (P.patch2patchinfo p)
                 return $ PatchChanges log (simplify [] $ map primToChange $ WO.unsafeUnFL (P.effect p))
  where simplify a [] = reverse a
        simplify a (c@(FileChange n t):cs) | t `elem` [FileAdded, FileRemoved, FileBinary]
          = simplify (c:filter (notFile n) a) (filter (notFile n) cs)
        simplify a ((FileChange n (FileHunk l f t)):cs)
          = simplify ((FileChange n (FileHunk l (if null f then f else highlight n f []) (if null t then t else highlight n t []))):a) cs
        simplify a (_:cs) = simplify a cs

        notFile n (FileChange { cfName = n' }) | n == n' = False
        notFile _ _ = True

primToChange :: Prim -> PatchChange
primToChange (Move f t) = Moved (fn2fp f) (fn2fp t)
primToChange (DP f t) = DirChange (fn2fp f) (fromDP t)
primToChange (FP f t) = FileChange (fn2fp f) (fromFP t)
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