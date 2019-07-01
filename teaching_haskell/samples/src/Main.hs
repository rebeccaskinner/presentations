module Main where

fib n
  | n <= 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

quicksort [] = []
quicksort (x:xs) =
  let lesser = filter (< x) xs
      greater = filter (>= x) xs
  in quicksort lesser ++ [x] ++ quicksort greater

main :: IO ()
main = do
  print (fib 5)
  print (quicksort [3,2,4,1])
