module PureImpure where
import Data.Foldable

reverseSentence :: String -> String
reverseSentence sentence =
  let
    wordsInReverse = reverse (words sentence)
  in unwords wordsInReverse

printSentenceBackwards :: IO ()
printSentenceBackwards = do
  sentence <- getLine
  let
    reversed = reverseSentence sentence
  putStrLn reversed

aBunchOfPrintStatements :: Int -> [IO ()]
aBunchOfPrintStatements count =
  [print x | x <- [0..count]]

printTenTimes :: IO ()
printTenTimes =
  for_ (aBunchOfPrintStatements 10) $ \statement -> do
    putStrLn "running a statement..."
    statement
