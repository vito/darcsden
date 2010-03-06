{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Session where

import Control.Monad.Reader
import Control.Monad.State
import Data.Data (Data)
import Data.Typeable (Typeable)
import Hack
import Happstack.State
import qualified Data.Map as M


data Notification = Success String | Message String | Warning String
                  deriving (Eq, Show, Typeable, Data)

data Session = Session { sID :: String
                       , sUser :: Maybe String
                       , sNotifications :: [Notification]
                       }
             deriving (Eq, Show, Typeable, Data)

newtype Sessions = Sessions (M.Map String Session)
                 deriving (Show, Typeable)

instance Version Notification
instance Version Session
instance Version Sessions

$(deriveSerialize ''Notification)
$(deriveSerialize ''Session)
$(deriveSerialize ''Sessions)

instance Component Sessions where
  type Dependencies Sessions = End
  initialValue = Sessions M.empty

getSession :: String -> Query Sessions (Maybe Session)
getSession id = asks (\(Sessions ss) -> M.lookup id ss)

addSession :: Session -> Update Sessions ()
addSession s = modify (\(Sessions ss) -> Sessions (M.insert (sID s) s ss))

updateSession :: Session -> Update Sessions ()
updateSession = addSession

deleteSession :: String -> Update Sessions ()
deleteSession id = modify (\(Sessions ss) -> Sessions (M.delete id ss))

$(mkMethods ''Sessions ['getSession, 'addSession, 'updateSession, 'deleteSession])

