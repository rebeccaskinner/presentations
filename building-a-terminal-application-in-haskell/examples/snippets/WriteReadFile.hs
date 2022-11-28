module WriteReadFile where

writeReadFile :: IO ()
writeReadFile =
  writeFile "example.txt" "Hello, Haskell"
  >>= (\_ -> readFile "example.txt")
  >>= (\contents -> print contents)
