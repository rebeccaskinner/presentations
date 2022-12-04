{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module HCatNoMetadata (runHCat) where
import Data.Char
import System.Environment
import System.IO
import System.Process
import Control.Monad
import Data.Foldable

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
    rows = screenRows dimensions - 2
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

-- showPages :: [String] -> IO ()
-- showPages allPages =
--   for_ allPages $ \page -> do
--     putStr "\^[[1J\^[[1;1H"
--     putStr page
--     cont <- getContinue
--     -- what now???
--     pure ()

-- showPages :: [String] -> IO ()
-- showPages =
--   showPages' Continue
--   where
--     showPages' Cancel _ = pure ()
--     showPages' Continue [] = pure ()
--     showPages' Continue (page:pages) = do
--       displayPage page
--       cont <- if null pages
--               then pure Cancel
--               else getContinue
--       showPages' cont pages
--     displayPage page = do
--       putStr "\^[[1J\^[[1;1H"
--       putStr page

-- showPages :: [String] -> IO ()
-- showPages [] = pure ()
-- showPages (page:pages) = do
--   putStr "\^[[1J\^[[1;1H"
--   putStr page
--   cont <- if null pages
--           then pure Cancel
--           else getContinue
--   when (Continue == cont) $
--     showPages pages

onContinue :: IO () -> IO ()
onContinue ioAction = do
  cont <- getContinue
  case cont of
    Cancel -> pure ()
    Continue -> ioAction

forPages :: (String -> IO ()) -> [String]  -> IO ()
forPages ioAction pages  =
  case pages of
    [] -> pure ()
    (page:rest) -> do
      ioAction page
      onContinue (forPages ioAction rest)

showPages :: [String] -> IO ()
showPages = forPages $ \page -> do
  putStr "\^[[1J\^[[1;1H"
  putStr page

-- showPages :: [String] -> IO ()
-- showPages [] = pure ()
-- showPages (page:pages) = do
--   putStr "\^[[1J\^[[1;1H"
--   putStr page
--   case pages of
--     [] -> pure ()
--     _otherwise ->
--   cont <- if null pages
--           then pure Cancel
--           else getContinue
--   when (Continue == cont) $
--     showPages pages

-- withContinue :: [String] -> (String -> IO ()) -> IO ()
-- withContinue [] _ = pure ()
-- withContinue (page:pages) f = do
--   f page
--   unless (null pages) $ do
--     cont <- getContinue
--     when (cont == Continue) $
--       withContinue pages f

-- showPages :: [String] -> IO ()
-- showPages pages = withContinue pages $ \page -> do
--   putStr "\^[[1J\^[[1;1H"
--   putStr page

-- withContinue :: [a] -> (a -> IO b) -> IO ()
-- withContinue [] _onItem = pure ()
-- withContinue (item:items) onItem = do
--   _ <- onItem item
--   unless (null items) $ do
--     cont <- getContinue
--     case cont of
--       Cancel -> pure ()
--       Continue -> withContinue items onItem

-- showPages :: [String] -> IO ()
-- showPages allPages =
--   withContinue allPages $ \page -> do
--     putStr "\^[[1J\^[[1;1H"
--     putStr page
