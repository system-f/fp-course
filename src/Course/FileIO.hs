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

-- bind, flatMap
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- effmap
-- (<$>) :: (a -> b) -> IO a -> IO b
-- apply (spaceship, angle bum, tie fighter)
-- (<*>) :: IO (a -> b) -> IO a -> IO b

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

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

-- IO a -> a

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \args ->
  case args of
    a:._ ->
      run a
    _ ->
      putStrLn "pass an arg silly"

-- did I violate my FP principles?


-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run filename =
  -- what is this?
  do
    contents <- readFile filename
    x <- getFiles (lines contents)
    printFiles x

{-

contents = readFile(filename);
x = getFiles(lines(contents));
return printFiles(x);

-}

{-

##### do-notation

* insert the word `do`
* turn `>>=` into `<-`
* delete `->`
* delete `\`
* swap each side of `<-`


  \filename ->
    readFile filename >>= \contents ->
    getFiles (lines contents) >>= \x ->
    printFiles x

-}

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . map getFile

-- \x -> f (g x)

-- List (IO (FilePath, Chars))
-- IO (List (FilePath, Chars))

-- sequence :: List (IO a) -> IO (List a)

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  \name -> (\contents -> (name, contents)) <$> readFile name

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  void . sequence . map (uncurry printFile)

-- \x -> f (g x)
-- f . g
-- \x -> f (g (h x))
-- f . g . h

-- (<$>) :: (a -> b) -> IO a -> IO b
-- sequence :: List (IO a) -> IO (List a)
-- void :: IO a -> IO ()

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents =
  putStrLn ("============ " ++ name) >>= \_ ->
  putStrLn contents

{-
(>>=) :: IO a -> (a -> IO b) -> IO b

============ share/c.txt
the contents of c
-}