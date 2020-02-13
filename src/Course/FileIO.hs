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

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- (<*>) :: IO (a -> b) -> IO a -> IO b
-- (<$>) ::    (a -> b) -> IO a -> IO b
-- sequence :: List (IO a) -> IO (List a)
-- etc

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile =
  \name contents ->
    putStrLn(flatten (replicate 10 "=-") ++ ' ' :. name) *>
    putStrLn(contents)
  {-
    do  {
          putStrLn("===== " ++ name);
          putStrLn(contents);
        }
        -}
-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
    -- \x -> void (sequence ((\(n, c) -> printFile n c) <$> x))
    -- void (sequence ((\(n, c) -> uncurry printFile (n, c)) <$> x))
    -- void (sequence (uncurry printFile <$> x))
  -- \x -> void (sequence ((<$>) (uncurry printFile) x))
  void . sequence . (<$>) (uncurry printFile)

-- \x -> f (g (h x))
-- f . g . h

-- List (IO ())
-- IO (List ())

-- void :: k a -> k ()
-- void :: IO (List ()) -> IO ()

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
  {-
  \name ->
    do  c <- readFile(name);
        return (name, c);
  -}

    -- (,) name <$> readFile name
    lift2 (<$>) (,) readFile

    -- readFile name >>= \c -> return ((,) name c)

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  \names ->
    sequence (getFile <$> names)

-- getFile :: FilePath -> IO (FilePath, Chars)
-- names :: List FilePath
-- List (IO (FilePath, Chars))

func :: (a -> b) -> (b -> c) -> (a -> c)
func f g x = g (f x)

deleteDatabase :: IO ()
deleteDatabase = error "todo"

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@, @lines@, and @printFiles@.
run ::
  FilePath
  -> IO ()
run =
  \p ->
    do  (_, c) <- getFile p
        r <- getFiles (lines c)
        printFiles r

data Pair a b = Two' a b | Three' a b b

snd' :: Pair a b -> b
snd' (Two' _ b) = b
snd' (Three' _ b _) = b

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        Nil ->
          putStrLn "pass args silly"
        h :. _ ->
          run h
          

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
