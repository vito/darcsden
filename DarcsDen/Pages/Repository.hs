{-# OPTIONS_GHC -F -pgmF trhsx #-}
module DarcsDen.Pages.Repository where

import HSP
import qualified Data.Map as M

import DarcsDen.Handler.Repository.Browse (RepoItem)
import DarcsDen.Handler.Repository.Changes (PatchLog, PatchChange)
import DarcsDen.Handler.Repository.Forks (Fork)
import DarcsDen.Pages.Base
import DarcsDen.State.Repository
import DarcsDen.State.User


init :: [(String, String)] -> HTMLPage
init _ _ = <div />

repo :: User -> Repository -> [RepoItem] -> String -> [RepoItem] -> Maybe String -> HTMLPage
repo _ _ _ _ _ _ _ = <div />

edit :: Repository -> [(String, String)] -> HTMLPage
edit r is _ = <div />

delete :: Repository -> HTMLPage
delete r _ = <div />

fork :: Repository -> String -> HTMLPage
fork r n _ = <div />

forks :: Repository -> [Fork] -> HTMLPage
forks r fs _ = <div />

changes :: User -> Repository -> [PatchLog] -> Int -> Int -> HTMLPage
changes u r cs p tp _ = <div />

changesAtom :: User -> Repository -> [PatchLog] -> String -> HTMLPage
changesAtom u r cs b _ = <div />

blob :: User -> Repository -> [RepoItem] -> String -> HTMLPage
blob u r p c _ = <div />

browse :: [Repository] -> Int -> Int -> HTMLPage
browse rs p tp _ = <div />

patch :: User -> Repository -> PatchLog -> [M.Map String String] -> [PatchChange]-> HTMLPage
patch u r c s cs _ = <div />


