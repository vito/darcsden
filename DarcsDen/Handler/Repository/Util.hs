module DarcsDen.Handler.Repository.Util where

import Data.Char (chr)
import System.FilePath (takeExtension)
import Text.Highlighting.Kate
import Text.XHtml.Strict (renderHtmlFragment)
import qualified Darcs.Patch as P
import qualified Darcs.Repository as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS


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

fromBS :: BS.ByteString -> String
fromBS = map (chr . fromIntegral) . BS.unpack

fromLS :: LS.ByteString -> String
fromLS = map (chr . fromIntegral) . LS.unpack

