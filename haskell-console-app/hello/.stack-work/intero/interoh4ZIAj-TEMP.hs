module App where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Fail

-- | The AppT monad represents the application state
newtype AppT m a = AppT
  { runAppT :: ExceptT AppException (ReaderT Cfg m) a
  } deriving (Functor,Applicative,Monad,MonadIO,MonadReader Cfg, MonadError AppException)

raiseAppException :: (Monad m, MonadFail m) => Either AppException a -> m a
raiseAppException = \case
  Left err -> Control.Monad.Fail.fail . show $ err
  Right a -> return a

runApp :: (Monad m, MonadFail m) => Cfg -> AppT m a -> m a
runApp cfg =
  (>>= raiseAppException) . flip runReaderT cfg . runExceptT . runAppT

-- | Add exception types here
data AppException = GeneralException String deriving (Eq, Show)

-- | Cfg contains the configured state of the application
data Cfg = Cfg

defaultConfig :: Cfg
defaultConfig = Cfg
