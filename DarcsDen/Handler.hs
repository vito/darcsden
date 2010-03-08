module DarcsDen.Handler where

import Hack
import Hack.Contrib.Mime
import Happstack.State
import System.Directory (doesFileExist, canonicalizePath, makeRelativeToCurrentDirectory)
import Data.ByteString.Lazy.Char8 (unpack, split, pack)
import Data.List (intercalate, isPrefixOf)
import System.FilePath (takeExtension)
import qualified Data.ByteString.Lazy as LS

import DarcsDen.HackUtils
import DarcsDen.Handler.Repository
import DarcsDen.Handler.User
import DarcsDen.State.Session
import DarcsDen.State.Repository


-- Pages
index :: Page
index s@(Session { sUser = Nothing }) e = doPage "index" [] s e
index s@(Session { sUser = Just n }) e
  = do repos <- query $ GetUserRepositories n
       doPage "index" [var "repositories" repos] s e

-- URL handling
handler :: Application
handler e = withSession e (\s -> pageFor path s e)
    where path = map unpack . split '/' . pack . tail . pathInfo $ e

pageFor :: [String] -> Page
pageFor [] = index
pageFor ["index"] = index
pageFor ["register"] = register
pageFor ["login"] = login
pageFor ["logout"] = logout
pageFor ["settings"] = settings
pageFor ["init"] = initialize
pageFor ("public":unsafe) = \s e ->
  do safe <- canonicalizePath ("public/" ++ intercalate "/" unsafe) >>= makeRelativeToCurrentDirectory
     exists <- doesFileExist safe

     -- Make sure there's no trickery going on here.
     if not ("public/" `isPrefixOf` safe && exists)
        then notFound s e
        else do

     let mime = maybe "text/plain" id $ lookup_mime_type (takeExtension safe)

     file <- LS.readFile safe
     return (Response 200 [("Content-Type", mime)] file)
pageFor [name] = user name
pageFor [name, repo] = repository name repo
pageFor (name:repo:"browse":file) = browseRepository name repo file
pageFor [name, repo, "changes"] = repositoryChanges name repo
pageFor [name, repo, "patch", p] = repositoryPatch name repo p
pageFor _ = notFound

