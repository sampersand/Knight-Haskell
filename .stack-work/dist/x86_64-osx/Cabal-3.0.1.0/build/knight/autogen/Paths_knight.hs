{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_knight (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/samp/me/Knight-Haskell/.stack-work/install/x86_64-osx/31371286d4136a6863a0193f9266296e307ecf3e3ef75f2628ec27b86eeee0a4/8.8.4/bin"
libdir     = "/Users/samp/me/Knight-Haskell/.stack-work/install/x86_64-osx/31371286d4136a6863a0193f9266296e307ecf3e3ef75f2628ec27b86eeee0a4/8.8.4/lib/x86_64-osx-ghc-8.8.4/knight-0.1.0.0-3ZK7OZY9Swj6cBeFB38ffa-knight"
dynlibdir  = "/Users/samp/me/Knight-Haskell/.stack-work/install/x86_64-osx/31371286d4136a6863a0193f9266296e307ecf3e3ef75f2628ec27b86eeee0a4/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/samp/me/Knight-Haskell/.stack-work/install/x86_64-osx/31371286d4136a6863a0193f9266296e307ecf3e3ef75f2628ec27b86eeee0a4/8.8.4/share/x86_64-osx-ghc-8.8.4/knight-0.1.0.0"
libexecdir = "/Users/samp/me/Knight-Haskell/.stack-work/install/x86_64-osx/31371286d4136a6863a0193f9266296e307ecf3e3ef75f2628ec27b86eeee0a4/8.8.4/libexec/x86_64-osx-ghc-8.8.4/knight-0.1.0.0"
sysconfdir = "/Users/samp/me/Knight-Haskell/.stack-work/install/x86_64-osx/31371286d4136a6863a0193f9266296e307ecf3e3ef75f2628ec27b86eeee0a4/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "knight_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "knight_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "knight_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "knight_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "knight_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "knight_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
