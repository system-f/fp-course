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

    (<$>) :: (a -> b) -> IO a -> IO b
    (<*>) :: IO (a -> b) -> IO a -> IO b
    (>>=) :: IO a -> (a -> IO b) -> IO b
    (=<<) :: (a -> IO b) -> IO a -> IO b
    pure :: a -> IO a



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
  c <- getChar
  print c
  printFiles results


fred :: Maybe String
fred = Just "fred"

barney :: Maybe String
barney = Just "barney"

count :: Maybe Int
count = Just 10

count' :: Maybe Int
count' = Nothing

attemptIt :: Int -> String -> Maybe [String]
attemptIt n name = 
  if n < 10
    then Just (replicate n name)
    else Nothing


example :: Maybe Int -> Maybe String -> Maybe Int
example mcount mname = do
  c <- mcount 
  n <- mname 
  names <- attemptIt c n
  pure (length names)


{-
  mcount >>= (\c -> -- mcount :: Maybe Int, c :: Int
  mname >>= (\n ->  -- mname :: Maybe String, n :: String
  attemptIt c n >>= (\names -> -- names :: [String]
  pure (length names))))
-}
  





{-
  readFile filename >>= \content ->
  getFiles (lines content) >>= \results ->
  getChar >>= \c ->
  print c >>= \_ ->
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

