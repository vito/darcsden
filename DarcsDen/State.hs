{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, TypeOperators #-}
module DarcsDen.State where

import Data.Typeable (Typeable)
import Happstack.State

import DarcsDen.State.User
import DarcsDen.State.Repository
import DarcsDen.State.Session


data State = State
             deriving (Typeable, Show)

instance Version State

$(deriveSerialize ''State)

instance Component State where
    type Dependencies State = Users :+: Repositories :+: Sessions :+: End
    initialValue = State

$(mkMethods ''State [])
