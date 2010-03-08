module Paths_press (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alex/.cabal/bin"
libdir     = "/home/alex/.cabal/lib/press-0.1.2/ghc-6.12.1"
datadir    = "/home/alex/.cabal/share/press-0.1.2"
libexecdir = "/home/alex/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "press_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "press_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "press_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "press_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
