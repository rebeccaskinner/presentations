module Main where
import Text.Printf

fibs = 0 : 1 : (zipWith (+) <*> tail) fibs

fib = (fibs !!)
fibSum a b = (fib a) + (fib b)

index1, index2, fib1, fib2 :: Int
index1 = 9
index2 = 11
fib1 = fib index1
fib2 = fib index2

main :: IO ()
main = do
  let s = fib1 + fib2
      s' = fibSum index1 index2

  putStrLn $ printf "fib(%d) = %d" index1 fib1
  putStrLn $ printf "fib(%d) = %d" index2 fib2
  putStrLn $ printf "We calculated:  %d + %d = %d" fib1 fib2 s
  putStrLn $ printf "Lib calculated: %d + %d = %d" fib1 fib2 s'
