module DarcsDen.Handler.Repository.Util where

import Data.Char (isAlphaNum)
import System.Exit
import System.FilePath (takeExtension)
import System.IO
import System.Process
import Text.XHtml.Strict (renderHtmlFragment)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R


getRepo :: String -> IO (Either String (R.Repository P.Patch))
getRepo = R.maybeIdentifyRepository []

highlight :: Bool -> String -> String -> IO String
highlight lineNums fn s = do
    (pin, pout, _, ph) <- runInteractiveCommand ("pygmentize " ++ unwords args)
    hPutStr pin s
    hClose pin
    res <- waitForProcess ph
    case res of
        ExitSuccess ->
            hGetContents pout
        ExitFailure _ ->
            return ("<div class=\"highlight\"><pre>" ++ renderHtmlFragment s ++ "</pre></div>")
  where
    args =
        [ "-l " ++ filter isAlphaNum (takeExtension fn)
        , "-f html"
        , if lineNums
             then "-O linenos,lineanchors=L,anchorlinenos"
             else ""
        ]

highlightBlob :: String -> String -> IO String
highlightBlob = highlight True

