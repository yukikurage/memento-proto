{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_memento_proto (
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
bindir     = "/home/yukikurage/projects/memento-proto/.stack-work/install/x86_64-linux/5a9b8d4a2eabed84974ac5ea689b7ffe788ecd380304cfa02fcce5386fc905d2/9.8.4/bin"
libdir     = "/home/yukikurage/projects/memento-proto/.stack-work/install/x86_64-linux/5a9b8d4a2eabed84974ac5ea689b7ffe788ecd380304cfa02fcce5386fc905d2/9.8.4/lib/x86_64-linux-ghc-9.8.4/memento-proto-0.1.0.0-9AYlP2SA6Xr5ogk2btjt8n"
dynlibdir  = "/home/yukikurage/projects/memento-proto/.stack-work/install/x86_64-linux/5a9b8d4a2eabed84974ac5ea689b7ffe788ecd380304cfa02fcce5386fc905d2/9.8.4/lib/x86_64-linux-ghc-9.8.4"
datadir    = "/home/yukikurage/projects/memento-proto/.stack-work/install/x86_64-linux/5a9b8d4a2eabed84974ac5ea689b7ffe788ecd380304cfa02fcce5386fc905d2/9.8.4/share/x86_64-linux-ghc-9.8.4/memento-proto-0.1.0.0"
libexecdir = "/home/yukikurage/projects/memento-proto/.stack-work/install/x86_64-linux/5a9b8d4a2eabed84974ac5ea689b7ffe788ecd380304cfa02fcce5386fc905d2/9.8.4/libexec/x86_64-linux-ghc-9.8.4/memento-proto-0.1.0.0"
sysconfdir = "/home/yukikurage/projects/memento-proto/.stack-work/install/x86_64-linux/5a9b8d4a2eabed84974ac5ea689b7ffe788ecd380304cfa02fcce5386fc905d2/9.8.4/etc"

getBinDir     = catchIO (getEnv "memento_proto_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "memento_proto_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "memento_proto_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "memento_proto_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memento_proto_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "memento_proto_sysconfdir") (\_ -> return sysconfdir)



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
