module Main where
import Viewer (runApp)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Read (readMaybe)
import Control.Monad ((>=>))

main :: IO ()
main = do
  port <- fromMaybe 8080 <$> getPortParam
  runApp port

-- getPortParam tries to get a parameter from the command line
getPortParam :: IO (Maybe Int)
getPortParam = (listToMaybe >=> readMaybe) <$> getArgs
