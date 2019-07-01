{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
module App where
import Numeric.Natural
import Data.Functor.Identity

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Fail

data HaskellMeetup = HaskellMeetup { people :: [String], snacks :: [String] }

type NumberBase = Natural

-- | The AppT monad represents the application state
newtype AppT m a = AppT
  { runAppT :: ReaderT Cfg m a
  } deriving (Functor,Applicative,Monad,MonadIO,MonadReader Cfg)


data Cfg = Cfg
