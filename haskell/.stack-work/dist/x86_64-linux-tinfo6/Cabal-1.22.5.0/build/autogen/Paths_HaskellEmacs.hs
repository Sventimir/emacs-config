module Paths_HaskellEmacs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sven/.emacs.d/haskell/.stack-work/install/x86_64-linux-tinfo6/e3c17d586d948e9173ddc7d31ed64617b8ec589d54b568a22d2a27e6483e0f09/7.10.3/bin"
libdir     = "/home/sven/.emacs.d/haskell/.stack-work/install/x86_64-linux-tinfo6/e3c17d586d948e9173ddc7d31ed64617b8ec589d54b568a22d2a27e6483e0f09/7.10.3/lib/x86_64-linux-ghc-7.10.3/HaskellEmacs-0.0.0-1O6TKvnkBmnDFjWhF6nSoO"
datadir    = "/home/sven/.emacs.d/haskell/.stack-work/install/x86_64-linux-tinfo6/e3c17d586d948e9173ddc7d31ed64617b8ec589d54b568a22d2a27e6483e0f09/7.10.3/share/x86_64-linux-ghc-7.10.3/HaskellEmacs-0.0.0"
libexecdir = "/home/sven/.emacs.d/haskell/.stack-work/install/x86_64-linux-tinfo6/e3c17d586d948e9173ddc7d31ed64617b8ec589d54b568a22d2a27e6483e0f09/7.10.3/libexec"
sysconfdir = "/home/sven/.emacs.d/haskell/.stack-work/install/x86_64-linux-tinfo6/e3c17d586d948e9173ddc7d31ed64617b8ec589d54b568a22d2a27e6483e0f09/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellEmacs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellEmacs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskellEmacs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellEmacs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellEmacs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
