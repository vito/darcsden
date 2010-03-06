{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module DarcsDen.State.Session where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char (chr)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Hack
import Happstack.State
import System.Random
import qualified Data.Map as M

import DarcsDen.HackUtils


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

sessID :: IO String
sessID = replicateM 32 randomAlphaNum
  where randomAlphaNum = do which <- randomRIO (0, 2 :: Integer)
                            random which
        random 0 = randomRIO (48, 57) >>= return . chr
        random 1 = randomRIO (97, 122) >>= return . chr
        random 2 = randomRIO (65, 90) >>= return . chr

withSession :: Env -> (Session -> IO Response) -> IO Response
withSession e r = case M.lookup "DarcsDenSession" cookies of
                    Nothing -> newSession r
                    Just id -> do ms <- query (GetSession id)
                                  case ms of
                                    Nothing -> newSession r
                                    Just s -> r s
    where cookies = readCookies e

newSession :: (Session -> IO Response) -> IO Response
newSession r = do id <- sessID
                  update $ AddSession (session id)
                  setCookies [("DarcsDenSession", id)] (r (session id))
    where session id = Session { sID = id
                               , sUser = Nothing
                               , sNotifications = []
                               }
