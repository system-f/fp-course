main :: IO ()
main = p1

p1 ::
  IO ()
p1 =
  let file = "/tmp/file"
      expr = readFile file
  in  writeFile file "abcdef" >>= \_ ->
      expr >>= \x ->
      putStrLn x >>= \_ ->
      writeFile file "ghijkl" >>= \_ ->
      expr >>= \y ->
      putStrLn (show (x, y))

  {-
  let file = "/tmp/file"
  in  do  _ <- writeFile file "abcdef"
          x <- readFile file
          _ <- putStrLn x
          _ <- writeFile file "ghijkl"
          y <- readFile file
          putStrLn (show (x, y))
          -}
  