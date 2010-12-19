module DarcsDen.Handler.Repository.Util where

import Control.Concurrent
import Data.Char (isAlphaNum)
import System.FilePath (takeExtension)
import System.IO
import System.Process
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R


getRepo :: String -> IO (Either String (R.Repository P.Patch))
getRepo = R.maybeIdentifyRepository []

highlight :: Bool -> String -> String -> IO String
highlight lineNums fn s =
    highlightAs (filter isAlphaNum (takeExtension fn)) $
        highlightAs "text" (return "Highlighting failed!")
  where
    args l =
        [ "-l " ++ l
        , "-f html"
        , if lineNums
             then "-O linenos,lineanchors=L,anchorlinenos"
             else ""
        ]

    highlightAs :: String -> IO String -> IO String
    highlightAs lexer err = do
        (pin, pout, _, ph) <- runInteractiveCommand ("pygmentize " ++ unwords (args lexer))
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
            _ -> err

highlightBlob :: String -> String -> IO String
highlightBlob = highlight True

