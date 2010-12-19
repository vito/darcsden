module DarcsDen.Handler.Repository.Util where

import Data.Char (isAlphaNum)
import System.Exit
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
        res <- waitForProcess ph
        case res of
            ExitSuccess -> hGetContents pout
            ExitFailure _ -> err

highlightBlob :: String -> String -> IO String
highlightBlob = highlight True

