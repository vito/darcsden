{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.Trans
import Database.CouchDB
import Snap.Http.Server
import System.Environment
import Text.JSON

import DarcsDen.Handler
import DarcsDen.State.Util


main :: IO ()
main = do
    putStrLn "checking couchdb..."
    runDB (return ())

    putStrLn "checking redis..."
    withRedis (return ())

    as <- getArgs
    case as of
        ("--install":_) -> runDB $ do
            liftIO (putStrLn "creating databases...")

            createDB "repositories"
            createDB "users"

            liftIO (putStrLn "creating repository design documents...")
            forM_ repoDesigns $ \js ->
                newDoc (db "repositories") js

            liftIO (putStrLn "creating user design documents...")
            forM_ userDesigns $ \js ->
                newDoc (db "users") js

            liftIO (putStrLn "All set!")

        ("--port":p:_) -> do
            putStrLn $ "darcsden running on port " ++ p
            startHTTP (read p)
        [] -> do
            putStrLn "darcsden running on port 8080"
            startHTTP 8080
        _ ->
            putStr . unlines $
                [ "usage:"
                , "  darcsden --install   : set up CouchDB databases"
                , "  darcsden --port PORT : start webserver on given port"
                , "  darcsden             : start webserver on port 8080"
                ]
  where
    startHTTP p = httpServe
        "*"
        p
        "127.0.0.1"
        (Just "/srv/darcs/access.log")
        (Just "/srv/darcs/error.log")
        handler

    repoDesigns =
        [ jsobj
            [ ("_id", jsstr "_design/repositories")
            , ("language", jsstr "javascript")
            , ("views", jsobj
                [ ("by_owner_and_name", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (doc.owner && doc.name && !doc.is_private)\n    emit([doc.owner, doc.name], doc);\n}")
                    ])

                , ("by_owner", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (doc.owner && !doc.is_private)\n    emit(doc.owner, doc);\n}")
                    ])

                , ("by_fork", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (doc.fork_of.Just && !doc.is_private)\n    emit(doc.fork_of.Just, doc);\n}")
                    ])

                , ("by_member", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (!doc.is_private)\n    for (member in doc.members)\n      emit(doc.members[member], doc._id);\n}")
                    ])
                ])
            ]

        , jsobj
            [ ("_id", jsstr "_design/private")
            , ("language", jsstr "javascript")
            , ("views", jsobj
                [ ("by_owner", jsobj
                    [ ("map", jsstr "function(doc) {\n  emit(doc.owner, doc);\n}\n")
                    ])
                , ("by_owner_and_name", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (doc.owner && doc.name)\n    emit([doc.owner, doc.name], doc);\n}")
                    ])
                , ("by_member", jsobj
                    [ ("map", jsstr "function(doc) {\n  for (member in doc.members)\n    emit(doc.members[member], [doc.owner, doc.name]);\n}")
                    ])
                ])
            ]
        ]

    userDesigns =
        [ jsobj
            [ ("_id", jsstr "_design/users")
            , ("language", jsstr "javascript")
            , ("views", jsobj
                [ ("by_email", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (doc.email)\n    emit(doc.email, doc);\n}")
                    ])
                , ("by_name", jsobj
                    [ ("map", jsstr "function(doc) {\n  if (doc.name)\n    emit(doc.name, doc);\n}")
                    ])
                ])
            ]
        ]

    jsstr = JSString . toJSString
    jsobj = JSObject . toJSObject

