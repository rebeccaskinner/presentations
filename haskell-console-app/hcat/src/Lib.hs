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

data PagerAction = PageNext | PagePrevious | Exit

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

mkSparsePage :: Int -> [Text.Text] -> Text.Text
mkSparsePage pageSize pageLines =
  let lineCount = length pageLines
      extraLines = Text.replicate (pageSize - lineCount) "\n"
      joined = mconcat $ (List.intersperse "\n" pageLines)
  in joined <> extraLines

statusBar :: FilePath -> Int -> Int -> Int -> Text.Text
statusBar filename width maxPages currentPage  =
  let
    startSeq = faceSeq [7]
    endSeq = faceSeq [0]
    ctrlLen = Text.length $ startSeq <> endSeq
    width' = width + ctrlLen
    pageCounter = Text.pack $
                  Printf.printf "(%d/%d)" currentPage maxPages
    bname = Text.pack . FilePath.takeBaseName $ filename
    barWidth = Text.length pageCounter + Text.length bname
    paddingAmount = width - (min width barWidth)
    padding = Text.replicate paddingAmount " "
  in Text.take width' $ Monoid.mconcat [ startSeq
                                       , bname
                                       , padding
                                       , pageCounter
                                       , endSeq
                                      ]

libMain :: IO ()
libMain = App.defaultConfig >>= (flip App.runApp libMain')
  where
    libMain' = do
      width   <- Reader.asks App.cfgTermWidth
      height  <- Reader.asks App.cfgTermHeight
      fname   <- getFilename
      txt     <- (IO.liftIO . Text.readFile) fname

      let textHeight = height - 2
          inputLines = Text.lines txt
          wrapped = concatMap (wordWrap width) inputLines
          paginated = pagesOf textHeight wrapped
          pages = map (mkSparsePage textHeight) paginated
          pageCnt = length pages
          statusBars = map (statusBar fname width pageCnt) [1..]
          addBar page bar = Text.unlines [page, bar]
          pagesWithStatusBar = zipWith addBar pages statusBars

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
    _   -> nextAction

paginate :: Zipper.Zipper Text.Text -> App.AppT IO ()
paginate pages = do
  let
    clearScreen :: App.AppT IO ()
    clearScreen = IO.liftIO $ do
      let clearStringCmd = screenSeq [2]
      IO.liftIO $ Text.putStr clearStringCmd

    currentPage = Maybe.fromMaybe "" (Zipper.get pages)

  clearScreen
  IO.liftIO $ Text.putStr currentPage
  action <- IO.liftIO nextAction
  case action of
    PageNext     -> Monad.unless (Zipper.isLast pages) $
                      paginate (Zipper.next pages)
    PagePrevious -> paginate (Zipper.prev pages)
    Exit         -> clearScreen
