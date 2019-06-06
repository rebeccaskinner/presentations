module Lib (libMain) where
import qualified App
import qualified Zipper
import qualified System.IO as IO
import qualified Control.Monad.Except   as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader   as Reader
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified System.Environment     as Env

data PagerAction = PageNext | PagePrevious | Exit deriving (Eq, Show)

getFilename :: App.AppT IO FilePath
getFilename = do
  args <- IO.liftIO $ Env.getArgs
  case args of
    []      -> Except.throwError App.MissingFileException
    [file]  -> return file
    allArgs -> Except.throwError (App.TooManyArgsException allArgs)

libMain :: IO ()
libMain = App.runApp App.defaultConfig $ do
  termWidth  <- Reader.asks App.cfgTermWidth
  termHeight <- Reader.asks App.cfgTermHeight
  inputText  <- (IO.liftIO . Text.readFile) =<< getFilename
  let wrapped = Zipper.mkZipper $ wordWrap termWidth inputText
  paginate wrapped

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap w txt = reverse $ wordWrap' [] w (Text.words txt)
  where
    wordWrap' :: [Text.Text] -> Int -> [Text.Text] -> [Text.Text]
    wordWrap' carry _ [] = carry
    wordWrap' [] w (word:words) = wordWrap' [word] w words
    wordWrap' (line:lines) w (word:words) =
      let line' = Text.unwords [line, word]
      in
        if Text.length line' <= w
        then wordWrap' (line':lines) w words
        else wordWrap' (word:line:lines) w words

nextAction :: IO PagerAction
nextAction = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False
  chr <- IO.hGetChar IO.stdin
  return $ case chr of
             'n' -> PageNext
             'p' -> PagePrevious
             'q' -> Exit

paginate :: Zipper.Zipper Text.Text -> App.AppT IO ()
paginate pages = do
  let msg = IO.liftIO . putStrLn
  action <- IO.liftIO nextAction
  case action of
    PageNext -> msg "next" >> paginate pages
    PagePrevious -> msg "prev" >> paginate pages
    Exit -> msg "end"
