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
  readFile :: Filename -> IO Chars
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

getNotArgs :: IO (List Chars)
getNotArgs = getNotArgs

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        x:._ -> run x
        _ -> putStrLn "please pass an argument"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run name =
  readFile name >>= \r ->
  getFiles (lines r) >>= \s ->
  -- (>>=) :: Monad f => f a -> (a -> f b) -> f b
  printFiles s


  {-
  do  
      r <- readFile name
      s <- getFiles (lines r)
      printFiles s
-}

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . (getFile <$>)
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  -- (\c -> (filename, c)) <$> readFile filename
  lift2 (<$>) (,) readFile 

  {-
  do  c <- readFile filename
      return (filename, c)
-}

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  {-}
printFiles Nil =
  pure ()
printFiles ((n, c):.t) =
  printFile n c *> printFiles t
-}
-- printFiles =
  -- foldRight (\(n, c) t -> printFile n c *> t) (pure ())
  void . sequence . (<$>) (uncurry printFile)

-- \x -> f (g (h x))
-- \x -> (f . g . h) x
-- f . g . h


-- (a -> b -> c) -> (a, b) -> c

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile filename contents =
  {-}
  (\_ -> putStrLn contents) =<< putStrLn ("============" ++ filename)
  -}
  putStrLn ("============" ++ filename) *>
  putStrLn contents

  {-
  putStrLn ("============" ++ filename) >>= \_ ->
  putStrLn contents
-}

{-
  do  putStrLn ("============" ++ filename)
      putStrLn contents
  -}

{-
void printFile(filename, contents) {
  putStrLn("============" ++ filename);
  putStrLn(contents);
}
-}
