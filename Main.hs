module Main where

import Hack
import Hack.Contrib.Press (renderToResponse)
import Hack.Handler.Happstack (run)
import Text.JSON.Generic

import DarcsDen.Handler

main :: IO ()
main = run handler