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

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do  a <- getArgs
      headOr (putStrLn "giz some args pls") (run <$> a)
  {-
  do  a <- getArgs
      case a of
        Nil -> putStrLn "giz some args pls"
        h:._ -> run h
  -}

type FilePath =
  Chars

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run n =
  do  c <- readFile n
    --string c = readFile(n);
      x <- getFiles (lines c)
    --sdfsdf x = getFiles(lines(n));
      printFiles x
    --printFiles(x);

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  traversee getFile 
  -- sequence . (<$>) getFile

-- \x -> f (g x)
-- \x -> (f . g) x
-- f . g

-- Given a file name, return (file name and file contents).
-- Use @readFile@
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  lift2 (<$>) (,) readFile

-- \x -> f (g x) (h x)
-- \x -> lift2 f g h x
-- lift2 f g h


-- \f g x -> f x (g x)
-- \x -> (f <*> g) x
-- f <*> g

-- \x -> f (g x) (h x) (i x)
-- \x -> lift3 f g h i x
-- lift3 f g h i

-- lift2 :: (a -> b -> c) -> f a -> f b -> f c
-- lift2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)

  {-
  do  f <- readFile filename
      pure (filename, f)
  -}
--readFile filename >>= \f -> pure (filename, f)
-- (\f -> (filename, f)) <$> readFile filename

-- Given a list of (file name and file contents), print each.
-- Use @printFile@
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
-- \x -> void (sequence ((<$>) (uncurry printFile) x))
  -- void . sequence . (<$>) (uncurry printFile)
  traverse_ (uncurry printFile)
 
-- \x -> f (g (h x))
-- f . g . h

-- Given the file name, and file contents, print them.
-- Use @putStrLn@
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile filename contents =
  {-
  let list = (("====== " ++ filename) :. contents :. Nil)
  in  traverse_ putStrLn list -- void (sequence (putStrLn <$> list))
  -}

  {-
  putStrLn ("====== " ++ filename) *>
  putStrLn contents
  -}

  {-
  putStrLn ("====== " ++ filename ++ "\n" ++ contents)
  -}

  {-
  putStrLn ("====== " ++ filename) >>= \_ ->
  putStrLn contents
  -}

  do
      putStrLn ("====== " ++ filename)
      putStrLn contents

traverse_ f =
  void . sequence . (<$>) f

traversee f = 
  sequence . (<$>) f

args = "arg1" :. "arg2" :. Nil

p :: List (IO ())
p = run <$> args

q :: IO ()
q = headOr (putStrLn "gimme args") p
