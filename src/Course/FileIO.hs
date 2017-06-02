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
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

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

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \a ->
  case a of
    Nil ->
      putStrLn "give me an argument silly"
    h:._ ->
      run h

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run name =
  do  c <- readFile name
      z <- getFiles (lines c)
      printFiles z

  {-
  readFile name >>= \c ->
  getFiles (lines c) >>= \z ->
  printFiles z
  -}

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . (<$>) getFile
  -- :: List (IO (FilePath, Chars))

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  -- \name -> (<$>) ((,) name) (readFile name)
  lift2 (<$>) (,) readFile

-- \x    -> f     (g   x   ) (h        x)
--lift2 f g h

{-
  readFile name >>= \c ->
  pure (name, c)
-}

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  -- ((a, b) -> c) -> a -> b -> c
  -- (a -> b -> c) -> (a, b) -> c
  \x -> void (sequence ((<$>) (uncurry printFile) x)) -- :: List (IO ())
  -- void . sequence . (<$>) (uncurry printFile)
  -- 1. turn List (IO ()) into a IO (List ()), how do we do that?
  -- 2. turn IO (List ()) into IO (), wedo that with void

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents =
  putStrLn ("======= " ++ name) *> putStrLn contents

{-

hack = seq . length

p2 =
  do  f <- readFile "/tmp/afile"
      f `hack` writeFile "/tmp/afile" "abc"
      g <- readFile "/tmp/afile"
      pure (f ++ g)

p = readFile "/tmp/afile"

p1 =
  do  f <- p
      f `hack` writeFile "/tmp/afile" "abc"
      g <- p
      pure (f ++ g)

-}



















