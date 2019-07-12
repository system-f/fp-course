{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

  (>>=) :: IO a -> (a -> IO b) -> IO b
  (<$>) :: (a -> b) -> IO a -> IO b

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents =
  do  {
        putStrLn("======== " ++ name);
        putStrLn(contents);
      }

      {-
  putStrLn name >>= \_ ->
  putStrLn contents
-}

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
-- \listofp -> void (sequence ((\(n, c) -> printFile n c) <$> listofp))
-- \listofp -> void (sequence ((uncurry printFile) <$> listofp))
-- \listofp -> void (sequence ((<$>) (uncurry printFile) listofp))
  void . sequence . (<$>) (uncurry printFile)

-- \x -> f (g (h x))
-- f . g . h


-- List (IO ()) -> IO (List ())


-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  {-
  do  chs <- readFile(name);
      return (name, chs);
-}

-- \name -> (\chs -> (name, chs)) <$> readFile name
  lift2 (<$>) (,) readFile

  -- \name -> readFile name >>= \chs -> return (name, chs)

-- readFile :: FilePath -> IO Chars
-- (>>=) :: IO a -> (a -> IO b) -> IO b

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  \list -> sequence (getFile <$> list)

-- List (IO (FilePath, Chars))
-- getFile :: FilePath -> IO (FilePath, Chars)


-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run =
  \name ->
    do  chs <- readFile name
        x <- getFiles (lines chs)
        printFiles x
  {-
     readFile name >>= \chs ->
     getFiles (lines chs) >>= \x ->
     printFiles x
  -}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \args ->
  case args of
    Nil -> putStrLn "pass an argument"
    h:._ -> run h

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
