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

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile =
  {-\name contents ->
    putStrLn ("=========== " ++ name) *>
    putStrLn contents -}
  \name contents ->
    putStrLn ("=========== " ++ name ++ "\n" ++ contents)

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  -- \list ->
    -- :: List (IO ())
    -- :: IO (List ())
    -- void (sequence ((<$>) (uncurry printFile) list))
  sequencevoid . ((<$>) (uncurry printFile))

sequencevoid = void . sequence

-- \x -> f (g (h x))
-- f . g . h

-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile =
-- \name -> readFile name >>= \cs -> pure (name, cs)
  -- Chars -> (FilePath, Chars)
  -- (\x -> (name, x)) <$> readFile name
  -- (\x -> (,) name x) <$> readFile name
  -- ((,) name) <$> readFile name
  --  \name -> (<$>) ((,) name) (readFile name)
  -- \name -> lift2 (<$>) (,) readFile name
  lift2 (<$>) (,) readFile

-- readFile name :: IO Chars

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.

sdfsf :: List FilePath -> List (IO (FilePath, Chars))
sdfsf = \list -> getFile <$> list

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  -- \list -> sequence ((<$>) getFile list)
  traversee getFile

traversee f = sequence . (<$>) f

-- \x -> f (g x)
-- f . g

    -- List (IO (FilePath, Chars))


-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run =
  \name ->
    do  cs <- readFile name
        a <- getFiles (lines cs)
        printFiles a
        {-
  \name ->
    readFile name >>= \cs ->
    getFiles (lines cs) >>= \a ->
    printFiles a
    -}
    -- IO (List (FilePath, Chars))


-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \a ->
  case a of
    Nil -> putStrLn "pass args"
    h:._ -> run h
    

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
