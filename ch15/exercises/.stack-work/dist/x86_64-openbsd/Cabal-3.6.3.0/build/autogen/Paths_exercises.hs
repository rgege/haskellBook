{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_exercises (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/xilo/Documents/programming/haskell/haskellBook/ch15/exercises/.stack-work/install/x86_64-openbsd/edf057471e19e5c0daa9b8ea5a3967c74270a67a0b43a44ca7246a18ead5599d/9.2.4/bin"
libdir     = "/home/xilo/Documents/programming/haskell/haskellBook/ch15/exercises/.stack-work/install/x86_64-openbsd/edf057471e19e5c0daa9b8ea5a3967c74270a67a0b43a44ca7246a18ead5599d/9.2.4/lib/x86_64-openbsd-ghc-9.2.4/exercises-0.1.0.0-3AHYkR1u9M1DTcoDqLZTFs"
dynlibdir  = "/home/xilo/Documents/programming/haskell/haskellBook/ch15/exercises/.stack-work/install/x86_64-openbsd/edf057471e19e5c0daa9b8ea5a3967c74270a67a0b43a44ca7246a18ead5599d/9.2.4/lib/x86_64-openbsd-ghc-9.2.4"
datadir    = "/home/xilo/Documents/programming/haskell/haskellBook/ch15/exercises/.stack-work/install/x86_64-openbsd/edf057471e19e5c0daa9b8ea5a3967c74270a67a0b43a44ca7246a18ead5599d/9.2.4/share/x86_64-openbsd-ghc-9.2.4/exercises-0.1.0.0"
libexecdir = "/home/xilo/Documents/programming/haskell/haskellBook/ch15/exercises/.stack-work/install/x86_64-openbsd/edf057471e19e5c0daa9b8ea5a3967c74270a67a0b43a44ca7246a18ead5599d/9.2.4/libexec/x86_64-openbsd-ghc-9.2.4/exercises-0.1.0.0"
sysconfdir = "/home/xilo/Documents/programming/haskell/haskellBook/ch15/exercises/.stack-work/install/x86_64-openbsd/edf057471e19e5c0daa9b8ea5a3967c74270a67a0b43a44ca7246a18ead5599d/9.2.4/etc"

getBinDir     = catchIO (getEnv "exercises_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "exercises_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "exercises_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "exercises_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercises_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
