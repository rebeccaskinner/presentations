{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HCatString (runHCat) where

import qualified Data.ByteString as BS
import Data.Char
import Data.Time.Clock
import Data.Time.Format
import System.Directory
import System.Environment
import System.IO
import System.Process
import Text.Printf

data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  }
  deriving stock (Show)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  }
  deriving stock (Show)

clearTerm, reverseVideo, resetVideo :: String
clearTerm =  "\^[[1J\^[[1;1H"
reverseVideo = "\^[[7m"
resetVideo = "\^[[0m"

clearScreen :: IO ()
clearScreen = putStr clearTerm

formatFileInfo :: FileInfo -> Int -> Int -> Int -> String
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
  invertText statusLine
  where
    timestamp =
      formatTime defaultTimeLocale "%F %T" fileMTime
    permissionString =
      [ if fileReadable then 'r' else '-'
      , if fileWriteable then 'w' else '-'
      , if fileExecutable then 'x' else '-'
      ]
    statusLine =
      truncateStatusLine $
        printf
          "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
          filePath
          permissionString
          fileSize
          timestamp
          currentPage
          totalPages
    invertText inputStr =
      reverseVideo <> inputStr <> resetVideo
    truncateStatusLine t
      | maxWidth <= 3 = ""
      | length t > maxWidth =
          take (maxWidth - 3) t <> "..."
      | otherwise = t

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- getPermissions filePath
  mtime <- getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return
    FileInfo
      { filePath = filePath
      , fileSize = size
      , fileMTime = mtime
      , fileReadable = readable perms
      , fileWriteable = writable perms
      , fileExecutable = executable perms
      }

handleArgs :: IO FilePath
handleArgs = do
  args <- getArgs
  case args of
    [fname] -> pure fname
    [] -> ioError $ userError "no filename provided"
    _ -> ioError $ userError "multiple files not supported"

runHCat :: IO ()
runHCat = do
  targetFilePath <- handleArgs
  contents <- readFile targetFilePath
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath
  showPages $ paginate termSize finfo contents

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
  hd : groupsOf n tl
  where (hd, tl) = splitAt n elems

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

paginate :: ScreenDimensions -> FileInfo -> String -> [String]
paginate (ScreenDimensions rows cols) finfo text =
  zipWith (<>) pages statusLines
  where
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap cols) (lines text)
    pages = map (unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = length pages
    statusLines = map (formatFileInfo finfo cols pageCount) [1 .. pageCount]
    padTo lineCount rowsToPad = take lineCount $ rowsToPad <> repeat ""

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

whileContinue :: a -> (a -> IO (Maybe a)) -> IO ()
whileContinue initialValue ioAction = do
  nextValue <- ioAction initialValue
  cont <- getContinue
  case (cont, nextValue) of
    (Continue, Just nextValue') ->
      whileContinue nextValue' ioAction
    _otherwise -> pure ()

showPages :: [String] -> IO ()
showPages allPages = do
  whileContinue allPages nextPage
  clearScreen
  where
    nextPage [] =
      pure Nothing
    nextPage (page : pages) = do
      clearScreen
      putStr page
      pure $ nonEmpty pages
    nonEmpty xs
      | null xs = Nothing
      | otherwise = Just xs

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  ScreenDimensions <$> tput "lines" <*> tput "cols"
  where
    tput cmd = read . init <$> readProcess "tput" [cmd] ""
