module DarcsDen.Dirty where

import Control.Monad
import Control.Monad.Trans
import System.Cmd (rawSystem)
import System.Exit


data Perhaps a = Error String | Alright a
  deriving (Eq, Ord, Show)

newtype Monad m => Dirty m a = Dirty { dirty :: m (Perhaps a) }

instance Monad m => Monad (Dirty m) where
  return = Dirty . return . Alright
  fail = Dirty . return . Error
  x >>= f = Dirty $ do try <- dirty x
                       case try of
                         Error e -> return (Error e)
                         Alright a -> dirty (f a)

instance MonadTrans Dirty where
  lift = Dirty . liftM Alright

instance MonadIO m => MonadIO (Dirty m) where
  liftIO = lift . liftIO

instance Functor Perhaps where
  fmap f (Alright x) = Alright (f x)
  fmap _ (Error e) = Error e

instance Monad Perhaps where
  (Alright a) >>= b = b a
  (Error e) >>= _ = Error e

  (Alright _) >> b = b
  (Error e) >> _ = Error e

  return = Alright
  fail = Error


perhaps :: Perhaps a -> (a -> b) -> (String -> b) -> b
perhaps (Alright a) f _ = f a
perhaps (Error e) _ f   = f e

shell :: String -> [String] -> Dirty IO ()
shell c as = do res <- lift (rawSystem c as)
                case res of
                  ExitSuccess -> return ()
                  ExitFailure n -> fail (c ++ " failed with exit code " ++ show n)

io :: IO a -> Dirty IO a
io i = Dirty $ fmap Alright i `catch` (return . Error . show)
