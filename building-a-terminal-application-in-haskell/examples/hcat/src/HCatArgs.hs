module HCatArgs where
import System.Environment

targetFileName :: IO FilePath
targetFileName = do
  args <- getArgs
  case args of
    [filename] ->
      return filename
    _otherwise ->
      ioError $ userError "please provide a single filename"

main :: IO ()
main = do
  contents <- readFile =<< targetFileName
  putStrLn contents
