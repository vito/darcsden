module Paths_highlighting_kate (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alex/.cabal/bin"
libdir     = "/home/alex/.cabal/lib/highlighting-kate-0.2.6/ghc-6.12.1"
datadir    = "/home/alex/.cabal/share/highlighting-kate-0.2.6"
libexecdir = "/home/alex/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "highlighting_kate_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "highlighting_kate_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "highlighting_kate_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "highlighting_kate_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
