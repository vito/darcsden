{-# LANGUAGE DeriveDataTypeable #-}
module DarcsDen.Handler.Repository where

import Data.Char (chr)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.List (intercalate, sort)
import Data.Map ((!))
import Hack
import Happstack.State
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import System.Time (getClockTime)
import Text.Highlighting.Kate
import Text.XHtml.Strict (renderHtmlFragment)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Darcs.Repository.InternalTypes as RI
import qualified Storage.Hashed.AnchoredPath as A
import qualified Storage.Hashed.Tree as T
import qualified Data.ByteString as BS

import DarcsDen.HackUtils
import DarcsDen.State.Repository
import DarcsDen.State.Session
import DarcsDen.State.User
import DarcsDen.Validate


data RepoItem = RepoItem { iIsDirectory :: Bool
                         , iName :: String
                         }
                deriving (Eq, Show, Data, Typeable)

instance Ord RepoItem where
  compare (RepoItem True _) (RepoItem False _) = LT
  compare (RepoItem False _) (RepoItem True _) = GT
  compare (RepoItem _ a) (RepoItem _ b) = compare a b

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
  = validate e [ io "repository does not exist" $ query (GetRepository (un, rn)) >>= return . (/= Nothing)
               , io "repository invalid" $ do
                   mr <- R.maybeIdentifyRepository [] ("repos/" ++ un ++ "/" ++ rn)
                   case mr of
                     Left _ -> return False
                     Right _ -> return True
               ]
    (\(OK _) -> do
      Just r <- query (GetRepository (un, rn))
      Right dr <- R.maybeIdentifyRepository [] ("repos/" ++ un ++ "/" ++ rn)

      fs <- files dr f
      bl <- blob dr f
      case (fs, bl) of
        (Nothing, Nothing) -> notFound s e
        (Just fs', _) -> do
          doPage "repo" [ var "user" un
                        , var "reponame" rn
                        , var "repo" r
                        , var "files" fs'
                        , var "up" (if null f
                                      then ""
                                      else intercalate "/" (init f))
                        , var "path" (intercalate "/" f)
                        ] e
        (_, Just source) ->
          doPage "repo-blob" [ var "user" un
                             , var "reponame" rn
                             , var "blob" (highlight (last f) source)
                             ] e)
    (\(Invalid _) -> notFound s e)

files :: R.Repository P.Patch -> [String] -> IO (Maybe [RepoItem])
files r@(RI.Repo p _ _ _) f
  = do root <- withDirectory p (R.readRecorded r >>= T.expand)
       let tree = T.findTree root (A.AnchoredPath (map A.makeName f))
       case (if null f then Just root else tree) of
         Nothing -> return Nothing
         Just t -> let onelevel = filter (\(A.AnchoredPath x,_) -> length x == 1) (T.list t)
                   in return . Just . sort $ map (\(a, _) -> RepoItem (maybe False (const True) (T.findTree t a)) (fromBS . A.flatten $ a)) onelevel
    where

blob :: R.Repository P.Patch -> [String] -> IO (Maybe String)
blob (RI.Repo p _ _ _) f
  = let filename = fromBS . A.flatten $ A.AnchoredPath (map A.makeName f)
    in withDirectory p $ do
      c <- doesFileExist filename
      if c
        then readFile filename >>= return . Just
        else return Nothing

highlight :: String -> String -> String
highlight f s = case hl of
                     Right res -> renderHtmlFragment (formatAsXHtml [OptNumberLines] lang res)
                     Left _ -> s
    where langs = languagesByExtension (takeExtension f)
          lang = if null langs then "text" else (head langs)
          hl = if null langs
                 then Right (map (\l -> [([], l)]) (lines s))
                 else highlightAs lang s

fromBS :: BS.ByteString -> String
fromBS = map (chr . fromIntegral) . BS.unpack