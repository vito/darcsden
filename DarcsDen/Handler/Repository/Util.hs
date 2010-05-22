module DarcsDen.Handler.Repository.Util where

import Data.List (intercalate)
import System.FilePath (takeExtension)
import Text.Highlighting.Kate
import Text.XHtml.Strict (renderHtmlFragment)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R


getRepo :: String -> IO (Either String (R.Repository P.Patch))
getRepo = R.maybeIdentifyRepository []

highlight :: Bool -> String -> String -> String
highlight l f s | l = withLineNums (length (lines s)) highlighted
                | otherwise = highlighted
    where
        langs = languagesByExtension (takeExtension f)
        lang = if null langs then "text" else head langs
        plaintext = map ((:[]).((,)[])) . lines -- robot buddha?
        hl = if null langs
                then Right (plaintext s)
                else highlightAs lang s
        highlighted =
            case hl of
                 Right res -> toXHtml res
                 Left _ -> toXHtml (plaintext s)
        toXHtml =
            renderHtmlFragment .
            formatAsXHtml [] lang .
            map ((["lineBreak"], "") :)

withLineNums :: Int -> String -> String
withLineNums ls s =
    "<table class=\"sourceCode\"><tr>"
    ++ lineNumbers
    ++ "<td class=\"sourceCode\"><pre class=\"sourceCode text\"><code>"
    ++ s
    ++ "</code></pre></td></tr></table>"
    where
        lineNumbers =
            "<td class=\"lineNumbers\"><pre>"
            ++ intercalate "<br />" (map (lineNumber . show) [1..ls])
            ++ "</pre></td>"

        lineNumber n = "<a class=\"lineNumber\" href=\"#L" ++ n ++ "\" id=\"LN" ++ n ++ "\" rel=\"#L" ++ n ++ "\">" ++ n ++ "</a>"


highlightBlob :: String -> String -> String
highlightBlob = highlight True

