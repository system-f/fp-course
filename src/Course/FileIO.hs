{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
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

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getChar >>= \_ ->
  getArgs >>= \c ->
  case c of
    Nil -> putStrLn "pass an arg dummy"
    (h:._) -> run h
                
type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run f =
  {-
  readFile f >>= \c ->
  getFiles (lines c) >>= \l ->
  printFiles l
  -}
  do
    c <- readFile f
    l <- getFiles (lines c)
    printFiles l

-- Use getFile, sequence
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . (<$>) getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  lift2 (<$>) (,) readFile

  {-
  readFile f >>= \c ->
  pure (f, c)
-}

-- Use printFile, sequence, void
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  void . sequence . (<$>) (uncurry printFile)
  -- void (sequence ((<$>) (\(n, c) -> printFile n c) x))

-- (FilePath -> Chars -> IO ()) -> ((FilePath, Chars) -> IO ())
-- (a        -> b     -> c    ) -> ((a       , b    ) -> c)
-- (a -> b -> c) -> ((a, b) -> c)

  -- List (IO ())
  -- sequence -> IO (List ())
  -- void -> IO ()

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile p c =
  -- putStrLn ("=====" ++ p) >>= \_ ->
  -- putStrLn c
  -- putStrLn (exp p c)
  do putStrLn ("=====" ++ p)
     putStrLn c

exp :: FilePath -> Chars -> Chars
exp p c =
  "===== " ++ p ++ "\n" ++ c











