import System.Environment
import Control.Applicative
import Control.Monad

{-

Functions --

  getArgs :: IO [String]
  putStrLn :: String -> IO ()
  readFile :: String -> IO String
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


-- getArgs :: IO [String]
-- Hint: use getArgs and run
main :: IO ()
main = getArgs >>= \args ->
   case args of 
     [] -> putStrLn "not enough input"
     h : _ -> run h
--
-- Hint: use getFiles and printFiles
run :: String -> IO ()
run path = 
  readFile path >>= getFiles . lines >>= printFiles 

getFiles :: [FilePath] -> IO [(FilePath, String)]
getFiles paths = 
  --  sequence (getFile <$> paths)
  forM paths getFile

getFile :: FilePath -> IO (FilePath, String)
getFile path = 
--  (\content -> (path, content)) <$> readFile path
  (\content -> (path, content)) <$> readFile path

printFiles :: [(FilePath, String)] -> IO ()
printFiles files = 
  void (sequence ((<$>) (uncurry printFile) files))

printFile :: FilePath -> String -> IO ()
printFile  path content = 
--  void (sequence (putStrLn <$> [path, content]))
--  putStrLn path *> putStrLn content
--  putStrLn (unlines [path, content])
  putStrLn (path ++ "\n" ++ content)


ex :: String -> Maybe String
ex s = Just s

ex' :: String -> String -> Maybe (String, String)
ex' s t = Just (s, t)

ex'' :: Maybe (String, String) -> Maybe (String)
ex'' m =  (\(x, y) -> x ++ y) <$> m

example :: (String, Int) -> Int
example (s, i) = i

example' :: (String, Int) -> Int
example' x = case x of 
              (s, i) -> i

example'' :: (String, Int) -> Int
example'' x = snd x


