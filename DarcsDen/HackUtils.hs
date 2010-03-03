module DarcsDen.HackUtils where

import Hack (Env (..))
import Data.ByteString.Lazy.Char8 (unpack, split, pack, intercalate)
import Network.URI (unEscapeString)


getInput :: String -> Env -> Maybe String
getInput key = lookup key . getInputs

getInputs :: Env -> [(String, String)]
getInputs = map (\[k,v] -> (sanitize k, sanitize v)) . map (split '=') . split '&' . hackInput
            where sanitize = unEscapeString . unpack . intercalate (pack " ") . split '+'
