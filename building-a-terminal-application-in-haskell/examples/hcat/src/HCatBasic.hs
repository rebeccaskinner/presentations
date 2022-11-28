module HCatBasic where

hcat :: IO ()
hcat =
  readFile "example.txt" >>= putStrLn
