{-# LANGUAGE LambdaCase #-}
module App where
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader

-- | The AppT monad represents the application state
newtype AppT m a = AppT
  { runAppT :: ExceptT AppException (ReaderT Cfg m) a
  } deriving (Functor,Applicative,Monad,MonadIO,MonadReader Cfg, MonadError AppException)

raiseAppException :: (Monad m, MonadFail m) => Either AppException a -> m a
raiseAppException = \case
  Left err -> Control.Monad.Fail.fail . exceptionReason $ err
  Right a -> return a

runApp :: (Monad m, MonadFail m) => Cfg -> AppT m a -> m a
runApp cfg =
  (>>= raiseAppException) . flip runReaderT cfg . runExceptT . runAppT

-- | Add exception types here
data AppException = GeneralException String
                  | MissingFileException
                  | TooManyArgsException [String]
                  deriving (Eq, Show)

exceptionReason :: AppException -> String
exceptionReason = \case
  GeneralException str      -> "General Exception: " <> str
  MissingFileException      -> "Missing Argument: Expected a file"
  TooManyArgsException args -> "Expected just 1 argument but got: " <> show args

-- | Cfg contains the configured state of the application
data Cfg = Cfg
  { cfgTermWidth  :: Int
  , cfgTermHeight :: Int
  }

defaultConfig :: Cfg
defaultConfig = Cfg { cfgTermWidth = 80, cfgTermHeight = 20 }
