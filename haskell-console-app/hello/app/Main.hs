module Main where
import qualified Lib

main :: IO ()
main = do
  fileName <- {-start-frag-}getFilename{-end-frag-}
  contents <- readFile fileName
  putStrLn contents
  {-start-frag-}
  where
    getFilename = do
      args <- System.Environment.getArgs
      return (head args)
  {-end-frag}
