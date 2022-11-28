{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module HCatNoMetadata (runHCat) where
import Data.Char
import System.Environment
import System.IO
import System.Process
import Control.Monad

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  }

getTerminalSize :: IO ScreenDimensions
getTerminalSize = do
  termLines <- tput TerminalLines
  termCols <- tput TerminalCols
  pure ScreenDimensions
    { screenRows = termLines
    , screenColumns = termCols
    }

data TerminalDimension
  = TerminalLines
  | TerminalCols

tput :: TerminalDimension -> IO Int
tput dimension = do
  outputData <- readProcess "tput" [cmd] ""
  pure . read . head . lines $ outputData
  where
    cmd =
      case dimension of
        TerminalLines -> "lines"
        TerminalCols -> "cols"

targetFileName :: IO FilePath
targetFileName = do
  args <- getArgs
  case args of
    [filename] ->
      pure filename
    _otherwise ->
      ioError $ userError "please provide a single filename"

runHCat :: IO ()
runHCat = do
  contents <- readFile =<< targetFileName
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  showPages $ paginate termSize contents

wordWrap :: Int -> String -> [String]
wordWrap lineLength lineText =
  case splitAt lineLength lineText of
    (fullLine, "") -> [fullLine]
    (hardwrappedLine, rest) ->
      let (nextLine, remainder) = softWrap hardwrappedLine
       in nextLine : wordWrap lineLength (remainder <> rest)
  where
    softWrap hardWrapped =
      let (rest, wrappedText) = break isSpace $ reverse hardWrapped
       in (reverse wrappedText, reverse rest)

paginate :: ScreenDimensions -> String -> [String]
paginate dimensions text = pages
  where
    rows = screenRows dimensions
    cols = screenColumns dimensions
    wrappedLines = concatMap (wordWrap cols) (lines text)
    pages = map (unlines . padTo rows) $ groupsOf rows wrappedLines
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""
    groupsOf n elems
      | null elems = []
      | otherwise =
        let (hd, tl) = splitAt n elems
        in hd : groupsOf n tl

data ContinueCancel
  = Continue
  | Cancel
  deriving stock (Eq, Show)

getContinue :: IO ContinueCancel
getContinue = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> return Continue
    'q' -> return Cancel
    _ -> getContinue

withContinue :: [a] -> (a -> IO b) -> IO ()
withContinue [] _onItem = pure ()
withContinue (item:items) onItem = do
  _ <- onItem item
  unless (null items) $ do
    cont <- getContinue
    case cont of
      Cancel -> pure ()
      Continue -> withContinue items onItem

showPages :: [String] -> IO ()
showPages allPages =
  withContinue allPages $ \page -> do
    putStr "\^[[1J\^[[1;1H"
    putStr page
