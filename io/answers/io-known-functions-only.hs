import System.Environment
import Control.Applicative
import Control.Monad

{-

Functions --

  getArgs :: IO [String]
  putStrLn :: IO String
  readFile :: String -> String
  lines :: String -> [String]
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure


Note, standard library lists --
  Cons (`:.`) is just `:`
  Nil is `[]`

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



main :: IO ()
main = getArgs >>= \args ->
  case args of
    file : [] ->
      run file
    _      ->
      putStrLn "usage: io <file>"

run :: String -> IO ()
run file =
  readFile file >>= \contents ->
    getFiles (lines contents) >>=
      printFiles

getFiles :: [String] -> IO [(String, String)]
getFiles files =
  sequence (getFile <$> files)

getFile :: String -> IO (String, String)
getFile name =
  (\content -> (name, content)) <$> readFile name

printFiles :: [(String, String)] -> IO ()
printFiles files =
  void $ sequence ((\(name, content) -> printFile name content) <$> files)

printFile :: String -> String -> IO ()
printFile name content =
  putStrLn ("============ " ++ name) >>
  putStrLn content
