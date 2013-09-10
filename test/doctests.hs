module Main where

import Build_doctests (deps)
import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

main ::
  IO ()
main =
  getSources >>= \sources -> doctest $
      "-isrc"
    : "-idist/build/autogen"
    : "-optP-include"
    : "-optPdist/build/autogen/cabal_macros.h"
    : "-hide-all-packages"
    : map ("-package="++) deps ++ sources

sourceDirectories ::
  [FilePath]
sourceDirectories =
  [
    "src"
  ]

isSourceFile ::
  FilePath
  -> Bool
isSourceFile p =
  and [takeFileName p /= "Setup.hs", isSuffixOf ".hs" p]

getSources :: IO [FilePath]
getSources =
  liftM (filter isSourceFile . concat) (mapM go sourceDirectories)
    where
      go dir = do
        (dirs, files) <- getFilesAndDirectories dir
        (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
