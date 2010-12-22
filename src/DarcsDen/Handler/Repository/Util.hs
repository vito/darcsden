module DarcsDen.Handler.Repository.Util where

import Control.Concurrent
import Darcs.Repository.Internal (IdentifyRepo(..))
import Darcs.Patch.V1 (Patch)
import Data.Char (isAlphaNum)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (takeExtension)
import System.IO
import System.Process
import Text.XHtml.Strict (renderHtmlFragment)
import qualified Darcs.Repository as R


getRepo :: String -> IO (Either String (R.Repository Patch r u t))
getRepo p = do
    r <- R.maybeIdentifyRepository [] p
    case r of
        GoodRepository r -> return (Right r)
        BadRepository s -> return (Left s)
        NonRepository s -> return (Left s)

highlight :: Bool -> String -> String -> IO String
highlight lineNums fn s =
    highlightAs (filter isAlphaNum (takeExtension fn)) $
        highlightAs "text" $ return . concat $
            [ "<div class=\"highlight\"><pre>"
            ,     renderHtmlFragment s
            , "</pre></div>"
            ]
  where
    args l =
        [ "-l " ++ l
        , "-f html"
        , if lineNums
             then "-O encoding=utf8,linenos,lineanchors=L,anchorlinenos"
             else "-O encoding=utf8"
        ]

    highlightAs :: String -> IO String -> IO String
    highlightAs lexer err = do
        (pin, pout, _, ph) <- runInteractiveCommand ("pygmentize " ++ unwords (args lexer))
        hSetEncoding pin utf8
        hPutStr pin s
        hClose pin

        reschan <- newChan
        waiter <- forkIO (hGetContents pout >>= writeChan reschan . Just)
        killer <- forkIO $ do
            threadDelay (10 * 1000000)
            killThread waiter
            terminateProcess ph
            writeChan reschan Nothing

        res <- readChan reschan
        mec <- getProcessExitCode ph
        killThread killer
        case (res, mec) of
            (Just out, Nothing) | length out > 0 ->
                return out
            (Just out, Just ExitSuccess) ->
                return out
            _ -> err

highlightBlob :: String -> String -> IO String
highlightBlob = highlight True

