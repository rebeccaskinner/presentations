module Lib (libMain) where
import qualified App
import qualified Control.Monad.IO.Class as IO

libMain :: IO ()
libMain = App.runApp App.defaultConfig $ do
  IO.liftIO $ putStrLn "Hello, World"


