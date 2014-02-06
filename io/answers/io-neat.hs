import System.Environment
import Control.Applicative
import Control.Monad


{-

Functions --

  putStrLn :: String -> IO ()
  readFile :: FilePath -> IO String
  lines :: String -> [String]
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    (<$>) :: (a -> b) -> f a -> f b
    (<*>) :: f (a -> b) -> f a -> f b
    (>>=) :: f a -> (a -> f b) -> f b
    (=<<) :: (a -> f b) -> f a -> f b
    pure :: a -> f a


  And the libraries that you know about:
    sequence
    join
    lift* (but in the standard library they are called liftA2, leftA3
           instead of just lift2 and lift3)

  Feel free to look for libraries that you can find out about yourself:
    - hoogle, search via the type signature you want.

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

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}


-- getArgs :: IO [String]
-- Hint: use getArgs and run
main :: IO ()
main = getArgs >>= \args ->
  case args of
    [filename] -> run filename
    _          -> putStrLn "usage: runhaskell io.hs filename"

--
-- Hint: use getFiles and printFiles
run :: String -> IO ()
run filename = do
  content <- readFile filename
  results <- getFiles (lines content)
  printFiles results

{-
  readFile filename >>= \content ->
  getFiles (lines content) >>= \results ->
  printFiles results
-}

getFiles :: [String] -> IO [(FilePath, String)]
getFiles =
  mapM getFile

getFile :: FilePath -> IO (FilePath, String)
getFile =
    -- \filename -> (<$>) ((,) filename) (readFile filename)
    liftA2 (<$>) (,) readFile

printFiles :: [(FilePath, String)] -> IO ()
printFiles =
  mapM_ $ uncurry printFile

printFile :: FilePath -> String -> IO ()
printFile filename contents =
--  mapM_ putStrLn ["======" ++ filename, contents]
  sequence_ (map putStrLn ["======" ++ filename, contents])
