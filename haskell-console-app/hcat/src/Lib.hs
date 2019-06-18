module Lib (libMain) where
import qualified App
import qualified Control.Monad          as Monad
import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import qualified Data.Char              as Char
import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import qualified Data.Monoid            as Monoid
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified System.Environment     as Env
import qualified System.FilePath        as FilePath
import qualified System.IO              as IO
import qualified Text.Printf            as Printf
import qualified Zipper

data PagerAction = PageNext | PagePrevious | Exit deriving (Eq, Show)

termSeq :: Char -> [Int] -> Text.Text
termSeq c codes =
  let codes' = List.intercalate ";" . map show $ codes
      escape = Char.chr 27
  in Text.pack $ Printf.printf "%c[%s%c" escape codes' c

faceSeq :: [Int] -> Text.Text
faceSeq = termSeq 'm'

screenSeq :: [Int] -> Text.Text
screenSeq = termSeq 'J'

getFilename :: App.AppT IO FilePath
getFilename = do
  args <- IO.liftIO $ Env.getArgs
  case args of
    []      -> Except.throwError App.MissingFileException
    [file]  -> return file
    allArgs -> Except.throwError (App.TooManyArgsException allArgs)

pagesOf :: Int -> [a] -> [[a]]
pagesOf cnt lst = reverse $ pagesOf' [] cnt lst
  where
    pagesOf' :: [[a]] -> Int -> [a] -> [[a]]
    pagesOf' carry cnt [] = carry
    pagesOf' carry cnt lst =
      let (hd,tl) = splitAt cnt lst
      in pagesOf' (hd:carry) cnt tl

mkSparsePage :: Int -> Text.Text -> Text.Text
mkSparsePage pageSize pageText =
  let missingLineCount = max 0 $ pageSize - (length $ Text.lines pageText)
      newlines = replicate missingLineCount "\n"
  in Monoid.mconcat (pageText : newlines)

statusBar :: FilePath -> Int -> Int -> Int -> Text.Text
statusBar filename width maxPages currentPage  =
  let startControlSequence = faceSeq [7]
      resetControlSequence = faceSeq [0]
      width' = width + (Text.length startControlSequence) + (Text.length resetControlSequence)
      longPageCountMsg = Text.pack $ Printf.printf "(%d/%d)" currentPage maxPages
      bname = Text.pack . FilePath.takeBaseName $ filename
      paddingAmount = width - (min width $ (Text.length longPageCountMsg) + (Text.length bname))
      padding = Text.replicate paddingAmount " "
  in Text.take width' $ Monoid.mconcat [ startControlSequence
                                       , bname
                                       , padding
                                       , longPageCountMsg
                                       , resetControlSequence
                                      ]

libMain :: IO ()
libMain = App.defaultConfig >>= (flip App.runApp libMain')
  where
    libMain' = do
      termWidth  <- Reader.asks App.cfgTermWidth
      termHeight <- Reader.asks App.cfgTermHeight
      fileName   <- getFilename
      inputText  <- (IO.liftIO . Text.readFile) fileName

      let textHeight = termHeight - 1
          inputLines = Text.lines inputText
          wrapped = concatMap (wordWrap termWidth) inputLines
          pages = map (mkSparsePage textHeight . Text.unlines) $ pagesOf textHeight wrapped
          statusBars = map (statusBar fileName termWidth (length pages)) [1..]
          pagesWithStatusBar = zipWith (\page bar -> Text.unlines [page, bar]) pages statusBars

      paginate (Zipper.mkZipper pagesWithStatusBar)

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap w txt =
  if Text.length txt < w
  then [txt]
  else
    let myOffset = Maybe.fromMaybe w (boundryOffset w txt)
        (thisLine, rest) = Text.splitAt myOffset txt
    in thisLine : wordWrap w rest
  where
    boundryOffset :: Int -> Text.Text -> Maybe Int
    boundryOffset 0 _ = Nothing
    boundryOffset idx text =
      if Text.index text idx == ' '
      then Just idx
      else boundryOffset (pred idx) text

nextAction :: IO PagerAction
nextAction = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False
  chr <- IO.hGetChar IO.stdin
  case chr of
    'n' -> return PageNext
    'p' -> return PagePrevious
    'q' -> return Exit
    _  -> nextAction

paginate :: Zipper.Zipper Text.Text -> App.AppT IO ()
paginate pages = do
  let
    msg :: String -> App.AppT IO ()
    msg = IO.liftIO . putStrLn

    clearScreen :: App.AppT IO ()
    clearScreen =
      let clearStringCmd = screenSeq [2]
      in IO.liftIO $ Text.putStr clearStringCmd

    currentPage = Maybe.fromMaybe "" (Zipper.get pages)

  clearScreen
  IO.liftIO $ Text.putStr currentPage

  action <- IO.liftIO nextAction
  case action of
    PageNext     -> Monad.unless (Zipper.isLast pages) $
                      paginate (Zipper.next pages)
    PagePrevious -> paginate (Zipper.prev pages)
    Exit         -> clearScreen
