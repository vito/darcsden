module DarcsDen.Dirty where

import Control.Monad
import Control.Monad.Trans
import System.Cmd (system)
import System.Exit


data Perhaps a = Error String | Alright a
  deriving (Eq, Ord)

newtype (Monad m) => Dirty m a = Dirty { dirty :: m (Perhaps a) }

instance Monad m => Monad (Dirty m) where
  return = Dirty . return . Alright
  fail = Dirty . return . Error
  x >>= f = Dirty $ do try <- dirty x
                       case try of
                         Error e -> return (Error e)
                         Alright a -> dirty (f a)

instance MonadTrans Dirty where
  lift = Dirty . (liftM Alright)

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


shell :: [String] -> IO a -> Dirty IO a
shell [] a = lift a
shell (c:cs) a = do res <- lift (system c)
                    case res of
                      ExitSuccess -> shell cs a
                      ExitFailure n -> fail (command c ++ " failed with exit code " ++ show n)
  where command = head . words

shell_ :: [String] -> Dirty IO ()
shell_ = flip shell (return ())
