module L03.Interactive where

import L03.Fuunctor
import L03.Moonad
import Data.Char
import Data.List(find)

-- | Eliminates any value over which a functor is defined.
vooid ::
  Fuunctor m =>
  m a
  -> m ()
vooid =
  fmaap (const ())

-- | A version of @bind@ that ignores the result of the effect.
(>-) ::
  Moonad m =>
  m a
  -> m b
  -> m b
(>-) a =
  (>>-) a . const

-- | An infix, flipped version of @bind@.
(>>-) ::
  Moonad m =>
  m a
  -> (a -> m b)
  -> m b
(>>-) =
  flip bind

-- | Runs an action until a result of that action satisfies a given predicate.
untilM ::
  Moonad m =>
  (a -> m Bool) -- ^ The predicate to satisfy to stop running the action.
  -> m a -- ^ The action to run until the predicate satisfies.
  -> m a
untilM p a =
  a >>- \r ->
  p r >>- \q ->
  if q
    then
      reeturn r
    else
      untilM p a

-- | Example program that uses IO to echo back characters that are entered by the user.
echo ::
  IO ()
echo =
  vooid (untilM
          (\c ->
            if c == 'q'
              then
                putStrLn "Bye!" >-
                reeturn True
              else
                reeturn False)
          (putStr "Enter a character: " >-
           getChar >>- \c ->
           putStrLn "" >-
           putStrLn [c] >-
           reeturn c))

data Op =
  Op
    Char -- ^ keyboard entry
    String -- ^ description
    (IO ()) -- ^ program

-- Exercise 1
-- * Ask the user to enter a string to convert to upper-case.
-- * Convert the string to upper-case.
-- * Print the upper-cased string to standard output.
-- ~~~ getLine :: IO String -- an IO action that reads a string from standard input.
-- ~~~ toUpper :: Char -> Char -- (Data.Char) converts a character to upper-case.
-- ~~~ putStr :: String -> IO () -- Prints a string to standard output.
-- ~~~ putStrLn :: String -> IO () -- Prints a string and then a new line to standard output.
convertInteractive ::
  IO ()
convertInteractive =
  error "todo"

-- Exercise 2
-- * Ask the user to enter a file name to reverse.
-- * Ask the user to enter a file name to write the reversed file to.
-- * Read the contents of the input file.
-- * Reverse the contents of the input file.
-- * Write the reversed contents to the output file.
-- ~~~ getLine :: IO String -- an IO action that reads a string from standard input.
-- ~~~ readFile :: FilePath -> IO String -- an IO action that reads contents of a file.
-- ~~~ writeFile :: FilePath -> String -> IO String -- writes a string to a file.
-- ~~~ reverse :: [a] -> [a] -- reverses a list.
-- ~~~ putStr :: String -> IO () -- Prints a string to standard output.
-- ~~~ putStrLn :: String -> IO () -- Prints a string and then a new line to standard output.
reverseInteractive ::
  IO ()
reverseInteractive =
  error "todo"

-- Exercise 3
-- * Ask the user to enter a string to url-encode.
-- * Convert the string with a URL encoder.
--   * For simplicity, encoding is defined as:
--     - ' ' -> "%20"
--     - '\t' -> "%09"
--     - '"' -> "%22"
--     - anything else is unchanged
-- * Print the encoded URL to standard output.
-- ~~~ toUpper :: Char -> Char -- (Data.Char) converts a character to upper-case.
-- ~~~ putStr :: String -> IO () -- Prints a string to standard output.
-- ~~~ putStrLn :: String -> IO () -- Prints a string and then a new line to standard output.
encodeInteractive ::
  IO ()
encodeInteractive =
  error "todo"

interactive ::
  IO ()
interactive =
  let ops = [
              Op 'c' "Convert a string to upper-case" convertInteractive
            , Op 'r' "Reverse a file" reverseInteractive
            , Op 'e' "Encode a URL" encodeInteractive
            , Op 'q' "Quit" (return ())
            ]
  in vooid (untilM
             (\c ->
               if c == 'q'
                 then
                   putStrLn "Bye!" >-
                   reeturn True
                 else
                   reeturn False)
             (putStrLn "Select: " >-
              traaverse (\(Op c s _) ->
                putStr [c] >-
                putStr ". " >-
                putStrLn s) ops >-
              getChar >>- \c ->
              putStrLn "" >-
              let o = find (\(Op c' _ _) -> c' == c) ops
                  r = case o of
                        Nothing -> (putStrLn "Not a valid selection. Try again." >-)
                        Just (Op _ _ k) -> (k >-)
              in r (return c)))
