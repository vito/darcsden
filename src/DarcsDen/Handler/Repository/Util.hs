module DarcsDen.Handler.Repository.Util where

import Control.Concurrent
import Darcs.Repository.Internal (IdentifyRepo(..))
import Darcs.Patch.V1 (Patch)
import Data.Char (isAlphaNum)
import Pygments
import Pygments.Formatters.Html
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (takeExtension)
import System.IO
import System.Process
import Text.Blaze (string)
import Text.Blaze.Renderer.Utf8
import qualified Darcs.Repository as R
import qualified Data.ByteString as BS

import DarcsDen.Util (strictLBS)


getRepo :: String -> IO (Either String (R.Repository Patch r u t))
getRepo p = do
    r <- R.maybeIdentifyRepository [] p
    case r of
        GoodRepository r -> return (Right r)
        BadRepository s -> return (Left s)
        NonRepository s -> return (Left s)

highlight :: Bool -> FilePath -> BS.ByteString -> BS.ByteString
highlight lineNums fn s =
    case lexer of
        Just l ->
            case runLexer l s of
                Right ts -> render ts
                Left _ -> render [Token Text s]
        Nothing -> render [Token Text s]
  where
    lexer = lexerFromFilename fn
    render = strictLBS . renderHtml . format lineNums

highlightBlob :: FilePath -> BS.ByteString -> BS.ByteString
highlightBlob = highlight True

