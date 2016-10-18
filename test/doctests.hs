module Main where

import Control.Applicative
import Prelude
import Build_doctests (deps)
import Control.Monad
import Data.List
import Data.Monoid
import System.Directory
import System.FilePath
import System.IO
import Test.DocTest

main ::
  IO ()
main =
  getSources >>= \sources ->
    forM_ (preferredOrderFirst sources) $ \source -> do
      hPutStrLn stderr $ "Testing " <> source
      doctest $
          "-isrc"
        : "-idist/build/autogen"
        : "-optP-include"
        : "-optPdist/build/autogen/cabal_macros.h"
        : "-hide-all-packages"
        : map ("-package="++) deps ++ [source]

sourceDirectories ::
  [FilePath]
sourceDirectories =
  [
    "src"
  ]

preferredOrderFirst :: [FilePath] -> [FilePath]
preferredOrderFirst sources =
     filter (`elem`    sources       ) preferredOrder
  <> filter (`notElem` preferredOrder) sources

-- If you find the tests are running slowly.
-- Comment out the Modules you have completed
-- in the list below.
preferredOrder :: [String]
preferredOrder = map (\f -> "src/Course" </> f <.> "hs") [
      "List"
    , "Functor"
    , "Applicative"
    , "Monad"
    , "FileIO"
    , "State"
    , "StateT"
    , "Extend"
    , "Comonad"
    , "Compose"
    , "Traversable"
    , "ListZipper"
    , "Parser"
    , "MoreParser"
    , "JsonParser"
    , "Interactive"
    , "Anagrams"
    , "FastAnagrams"
    , "Cheque"
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
