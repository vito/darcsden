module DarcsDen.Darcs where

import Control.Monad.Trans
import Darcs.Commands (DarcsCommand(commandCommand))
import Darcs.Flags (DarcsFlag(..))
import Darcs.Utils (withCurrentDirectory)
import System.Directory
import qualified Darcs.Commands.Get as G
import qualified Darcs.Repository as R


sanityDarcs :: DarcsCommand -> [DarcsFlag] -> [String] -> IO ()
sanityDarcs c fs as =
  withCurrentDirectory "." ((commandCommand c) (Quiet:fs) as)


get :: MonadIO m => String -> m ()
get from = liftIO $ sanityDarcs G.get [] [from]


getTo :: MonadIO m => String -> String -> m ()
getTo from to = liftIO $ sanityDarcs G.get [] [from, to]


init :: MonadIO m => String -> m ()
init to = liftIO $ do
  createDirectoryIfMissing True to
  withCurrentDirectory to (R.createRepository []) `catch` \_ ->
    removeDirectoryRecursive to
