{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module HCat where
import qualified System.Environment as Environment
import qualified Data.Maybe as Maybe
import qualified Text.Printf as Printf
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import System.IO
import qualified System.Info
import qualified System.Process as Process
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory

data FileInfo = FileInfo
  { filePath  :: FilePath
  , fileSize  :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving Show

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
  let
    timestamp =
      TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
    permissionString =
      [ if fileReadable then 'r' else '-'
      , if fileWriteable then 'w' else '-'
      , if fileExecutable then 'x' else '-' ]
    statusLine = Text.pack $
      Printf.printf
      "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
      filePath
      permissionString
      fileSize
      timestamp
      currentPage
      totalPages
  in invertText (truncateStatus statusLine)
  where
    invertText inputStr =
      let
        reverseVideo = "\^[[7m"
        resetVideo = "\^[[0m"
      in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
      | maxWidth <= 3 = ""
      | Text.length statusLine > maxWidth =
        Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return FileInfo
    { filePath = filePath
    , fileSize = size
    , fileMTime = mtime
    , fileReadable = Directory.readable perms
    , fileWriteable = Directory.writable perms
    , fileExecutable = Directory.executable perms
    }

data HCatSettings = HCatSettings
  { hcatLines :: Int
  , hcatFiles :: [FilePath]
  } deriving (Show)

defaultLineCount :: IO String
defaultLineCount =
  Maybe.fromMaybe "20"
  <$> Environment.lookupEnv "HCAT_LINES"

getFilenameFromArgs :: [String] -> Either String FilePath
getFilenameFromArgs =
  \case
    [fname] -> Right fname
    []      -> Left "no filename provided"
    _       -> Left "multiple files not supported"

handleArgs :: IO FilePath
handleArgs =
  parseArgs <$> Environment.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> fname
        []      -> fail "no filename provided"
        _       -> fail "multiple files not supported"

runHCat :: IO ()
runHCat = do
  targetFilePath <- handleArgs
  contents <- TextIO.readFile targetFilePath
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath
  let pages = paginate termSize finfo contents
  showPages pages

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
  in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
    let
      (candidate, nextLines) = Text.splitAt lineLength lineText
      (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
    in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText,Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
        let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
        in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
  let
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap cols) (Text.lines text)
    pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = length pages
    statusLines = map (formatFileInfo finfo cols pageCount) [1..pageCount]
  in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text.Text] -> [Text.Text]
    padTo lineCount rowsToPad =
      take lineCount $ rowsToPad <> repeat ""

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue =
  hSetBuffering stdin NoBuffering
  >> hSetEcho stdin False
  >> getChar
  >>= \case
    ' ' -> return Continue
    'q' -> return Cancel
    _   -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  shouldContinue <- getContinue
  case shouldContinue of
    Continue -> showPages pages
    Cancel   -> return ()

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case System.Info.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other ->
      ioError . userError
      $ Printf.printf "Unsupported platform: %s" System.Info.os
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      lines <- Process.readProcess "tput" ["lines"] ""
      cols  <- Process.readProcess "tput" ["cols"] ""
      return ScreenDimensions
          { screenRows = read . init $ lines
          , screenColumns = read . init $ cols
          }
