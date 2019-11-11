{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_presupposition (
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

bindir     = "/home/juliangrove/Documents/Haskell/presupposition/presupposition/.stack-work/install/x86_64-linux-tinfo6/021d25a811b3d15d5484c96364aa964b0bc6d48c349904b8860146d2ce55c2be/8.6.5/bin"
libdir     = "/home/juliangrove/Documents/Haskell/presupposition/presupposition/.stack-work/install/x86_64-linux-tinfo6/021d25a811b3d15d5484c96364aa964b0bc6d48c349904b8860146d2ce55c2be/8.6.5/lib/x86_64-linux-ghc-8.6.5/presupposition-0.1.0.0-FwrJ1qPbZum2Ssl3x4fV3C-presupposition"
dynlibdir  = "/home/juliangrove/Documents/Haskell/presupposition/presupposition/.stack-work/install/x86_64-linux-tinfo6/021d25a811b3d15d5484c96364aa964b0bc6d48c349904b8860146d2ce55c2be/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/juliangrove/Documents/Haskell/presupposition/presupposition/.stack-work/install/x86_64-linux-tinfo6/021d25a811b3d15d5484c96364aa964b0bc6d48c349904b8860146d2ce55c2be/8.6.5/share/x86_64-linux-ghc-8.6.5/presupposition-0.1.0.0"
libexecdir = "/home/juliangrove/Documents/Haskell/presupposition/presupposition/.stack-work/install/x86_64-linux-tinfo6/021d25a811b3d15d5484c96364aa964b0bc6d48c349904b8860146d2ce55c2be/8.6.5/libexec/x86_64-linux-ghc-8.6.5/presupposition-0.1.0.0"
sysconfdir = "/home/juliangrove/Documents/Haskell/presupposition/presupposition/.stack-work/install/x86_64-linux-tinfo6/021d25a811b3d15d5484c96364aa964b0bc6d48c349904b8860146d2ce55c2be/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "presupposition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "presupposition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "presupposition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "presupposition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "presupposition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "presupposition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
