{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module DarcsDen.Handler.Repository where

import Darcs.Patch.FileName (fn2fp)
import Darcs.Patch.Info (PatchInfo, pi_date, pi_name, pi_author, pi_log, make_filename)
import Darcs.Patch.Prim (Prim(..), DirPatchType(..), FilePatchType(..))
import Darcs.Utils (withCurrentDirectory)
import Data.Char (chr, isNumber, isSpace)
import Data.List (inits, intercalate, isPrefixOf, isSuffixOf, nub, sort)
import Data.Map ((!))
import Data.Maybe (fromJust, fromMaybe)
import Hack
import Happstack.State
import System.Directory (renameDirectory)
import System.FilePath (takeExtension)
import System.Time (getClockTime, calendarTimeToString)
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
import qualified Data.ByteString.Lazy.Char8 as LC

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


instance Ord RepoItem where
  compare (RepoItem _ _ True) (RepoItem _ _ False) = LT
  compare (RepoItem _ _ False) (RepoItem _ _ True) = GT
  compare (RepoItem a _ _) (RepoItem b _ _) = compare a b

handleRepo :: String -> String -> [String] -> Page
handleRepo un rn action s e
  = validate e [ io "repository does not exist" $ query (GetRepository (name, repo)) >>= return . (/= Nothing)
               , io "repository invalid" $ getRepo (repoDir name repo) >>= return . either (const False) (const True)
               ]
    (\(OK _) ->
      case action of
        [] -> browseRepo name repo [] s e
        ("_darcs":unsafe) -> serveDirectory (repoDir name repo ++ "/_darcs/") unsafe s e
        ("browse":file) -> browseRepo name repo file s e
        ["edit"] -> editRepo name repo s e
        ["delete"] -> deleteRepo name repo s e
        ["changes"] -> pagedRepoChanges name repo 1 s e
        ["changes", "page", page] | all isNumber page -> pagedRepoChanges name repo (read page :: Int) s e
        ["patch", patch] -> repoPatch name repo patch s e
        ["fork"] -> forkRepo name repo s e
        _ -> notFound s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
  where name = saneName un
        repo = saneName rn

initialize :: Page
initialize s@(Session { sUser = Nothing }) _ = warn "You must be logged in to create a repository." s >> redirectTo "/login"
initialize s e@(Env { requestMethod = GET }) = doPage "init" [] s e
initialize s@(Session { sUser = Just n }) e
  = validate e
    [ nonEmpty "name"
    , predicate "name" isSane "contain only alphanumeric characters, underscores, and hyphens"
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
    (\(Invalid f) -> do
        notify Warning s f
        doPage "init" [assocObj "in" (getInputs e)] s e)

browseRepo :: String -> String -> [String] -> Page
browseRepo un rn f s e = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))
  Right dr <- getRepo (repoDir un rn)

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
                    , var "isAdmin" (sUser s == Just (rOwner r))
                    ] s e
    (_, Just source) ->
      doPage "repo-blob" [ var "user" u
                         , var "repo" r
                         , var "file" (last path)
                         , var "blob" (highlight (last f) source [OptNumberLines])
                         , var "path" (init path)
                         , var "isAdmin" (sUser s == Just (rOwner r))
                         ] s e

pagedRepoChanges :: String -> String -> Int -> Page
pagedRepoChanges un rn page s e = do
  Just u <- query (GetUser un)
  Just r <- query (GetRepository (un, rn))

  (patches, totalPages) <- R.withRepositoryDirectory [] (repoDir un rn) $ \dr -> do
    pset <- R.read_repo dr
    let ps = WO.unsafeUnRL . WO.reverseFL . R.patchSetToPatches $ pset
    ls <- mapM (toLog . P.patch2patchinfo) . paginate $ ps
    return (ls, ceiling ((fromIntegral (length ps) :: Double) / 30))

  doPage "repo-changes" [ var "user" u
                        , var "repo" r
                        , var "patches" patches
                        , var "page" page
                        , var "totalPages" totalPages
                        , var "nextPage" (page + 1)
                        , var "prevPage" (page - 1)
                        , var "notFirst" (page /= 1)
                        , var "notLast" (page /= totalPages)
                        , var "isAdmin" (sUser s == Just (rOwner r))
                        ] s e
  where paginate = take 30 . drop (30 * (page - 1))

repoPatch :: String -> String -> String -> Page
repoPatch un rn p s e = do
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
                      , var "isAdmin" (sUser s == Just (rOwner r))
                      ] s e
  where summarize :: [[(String, JSValue)]] -> [PatchChange] -> [JSValue]
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

        modification (FileChange _ (FileHunk _ _ _)) = True
        modification _ = False

editRepo :: String -> String -> Page
editRepo un rn s e@(Env { requestMethod = GET })
 = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        Just r <- query (GetRepository (un, rn))
        ms <- members r
        doPage "repo-edit" [ var "repo" r
                           , var "members" ms
                           , var "isAdmin" True
                           ] s e)
    (\(Invalid f) -> notify Warning s f >> redirectTo "/")
editRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s)
    , nonEmpty "name"
    ]
    (\(OK _) -> do
        Just r <- query (GetRepository (un, rn))
        ms <- members r

        mapM_ (\m -> case getInput ("remove-" ++ m) e of
                  Nothing -> return False
                  Just _ -> removeMember m r) ms

        case getInput "add-members" e of
          Just "" -> return ()
          Just as -> mapM_ (\m -> do c <- query (GetUser $ LC.unpack m)
                                     case c of
                                       Just _ -> addMember (strip . LC.unpack $ m) r
                                       Nothing -> warn ("Invalid user; cannot add: " ++ LC.unpack m) s >> return False)
                     (LC.split ',' (LC.pack as))
          _ -> return ()

        Just s' <- query (GetSession (sID s)) -- may be some new warnings from user adding

        let newName = fromMaybe (rName r) (getInput "name" e)
        if rName r /= newName
          then do renameDirectory (repoDir un rn) (repoDir un (fromJust (getInput "name" e)))
                  update (DeleteRepository (un, rn))
          else return ()

        update (UpdateRepository (r { rName = newName
                                    , rDescription = fromMaybe (rDescription r) (getInput "description" e)
                                    , rWebsite = fromMaybe (rWebsite r) (getInput "website" e)
                                    }))

        success "Repository updated." s'

        redirectTo ("/" ++ un ++ "/" ++ newName ++ "/edit"))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ un ++ "/" ++ rn ++ "/edit"))
  where strip = strip' . strip'
        strip' = reverse . dropWhile isSpace

deleteRepo :: String -> String -> Page
deleteRepo un rn s e@(Env { requestMethod = GET })
 = validate e
   [ io "you do not own this repository" (return $ Just un == sUser s) ]
   (\(OK _) -> do
       Just r <- query (GetRepository (un, rn))
       doPage "repo-delete" [ var "repo" r
                            , var "isAdmin" True
                            ] s e)
   (\(Invalid f) -> notify Warning s f >> redirectTo "/")
deleteRepo un rn s e
  = validate e
    [ io "you do not own this repository" (return $ Just un == sUser s) ]
    (\(OK _) -> do
        destroyRepository (un, rn)
        success "Repository deleted." s
        redirectTo ("/" ++ un))
    (\(Invalid f) -> do
        notify Warning s f
        redirectTo ("/" ++ un ++ "/" ++ rn))

forkRepo :: String -> String -> Page
forkRepo _ _ s@(Session { sUser = Nothing }) _ = warn "You must be logged in to fork a repository." s >> redirectTo "/"
forkRepo un rn s@(Session { sUser = Just n }) _ = do
  Just r <- query (GetRepository (un, rn))

  forkRepository n r

  success "Repository forked." s
  redirectTo ("/" ++ n ++ "/" ++ rName r)


-- Helper functions (TODO: Probably put somewhere better.)
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