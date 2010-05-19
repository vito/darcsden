module DarcsDen.Handler.Repository.Util where

import System.FilePath (takeExtension)
import Text.Highlighting.Kate
import Text.XHtml.Strict (renderHtmlFragment)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R


getRepo :: String -> IO (Either String (R.Repository P.Patch))
getRepo = R.maybeIdentifyRepository []

highlight :: String -> String -> [FormatOption] -> String
highlight f s os = case hl of
                     Right res -> renderHtmlFragment (formatAsXHtml os lang res)
                     Left _ -> s
    where langs = languagesByExtension (takeExtension f)
          lang = if null langs then "text" else head langs
          hl = if null langs
                 then Right (map (\l -> [([], l)]) (lines s))
                 else highlightAs lang s

highlightBlob :: String -> String -> String
highlightBlob f s = highlight f s [OptNumberLines]

