{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module ThemeLoader where

import Control.Applicative
import Data.Aeson
import Data.Bifunctor
import Data.Char
import Data.Kind
import Data.List
import Data.Functor.Identity
import Control.Monad.Fail
import GHC.Generics
import Color
import ColorX11
import Text.Read
import qualified Data.Map.Strict as Map
import Control.Monad.Except
import Control.Monad.Reader

newtype ColorReference r a =
  ColorReference { unColorReference :: ExceptT String (Reader r) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadError String)

evalColorReference :: r -> ColorReference r a -> Either String a
evalColorReference env =
  flip runReader env . runExceptT . unColorReference

data ColorValue w
  = RGBValue RGB
  | X11Value SomeColor
  | OtherColor (HKD w (ColorValue w))

instance FromJSON (ColorValue (ColorReference RawThemeConfig)) where
  parseJSON = withObject "color" $ \val ->
    parseRGBElement val <|> parseX11Element val <|> parseRefElement val
    where
      parseRGBElement val = do
        t <- val .: "rgb"
        RGBValue <$> parseRGB t

      parseX11Element val = do
        t <- val .: "x11"
        X11Value <$> parseX11Color t

      parseRefElement val = do
        refName <- val .: "same-as"
        pure . OtherColor $ generateRef refName

      generateRef name = do
        colors <- asks (getThemeConfig . getRawThemeConfig)
        case Map.lookup name colors of
          Nothing -> throwError $ "Referenced color not found: " <> name
          Just color' -> pure color'

dereferenceColorValue
  :: RawThemeConfig
  -> ColorValue (ColorReference RawThemeConfig)
  -> Either String (ColorValue Identity)
dereferenceColorValue env colorValue =
  case colorValue of
    RGBValue r -> Right $ RGBValue r
    X11Value c -> Right $ X11Value c
    OtherColor r ->
      fmap OtherColor $
        evalColorReference env r >>= dereferenceColorValue env

type family HKD (wrapper :: Type -> Type) (value :: Type) :: Type where
  HKD Identity value = value
  HKD wrapper value = wrapper value

newtype RawThemeConfig = RawThemeConfig { getRawThemeConfig :: ThemeConfig' (ColorReference RawThemeConfig) }
newtype ThemeConfig' w = ThemeConfig'
  { getThemeConfig :: Map.Map String (ColorValue w) }

type ThemeConfig = ThemeConfig' Identity

evalConfig :: RawThemeConfig -> Either String ThemeConfig
evalConfig rawConfig =
  fmap ThemeConfig' .
  traverse (dereferenceColorValue rawConfig)  .
  getThemeConfig .
  getRawThemeConfig $ rawConfig

parseRGB :: MonadFail m => String -> m RGB
parseRGB s =
  case s of
    '#':s' -> parseRGB s'
    [r,r',g,g',b,b'] -> do
      RGB
        <$> parseHex r r'
        <*> parseHex g g'
        <*> parseHex b b'
    otherwise ->
      fail $ "invalid RGB hex string "
          <> s
          <> " expected a string in the form of #1a2b3c"
    where
      parseHex a b =
        let s = ['0','x',a,b]
        in case readEither s of
             Left err -> fail err
             Right val -> pure val

parseX11Color :: MonadFail m => String -> m SomeColor
parseX11Color s =
  let (ThemeInstance t) = x11Theme
      p (name,color) = s == name
  in case find p t of
       Nothing -> fail $ "no X11 color " <> s
       Just c -> pure $ snd c
  where
    downcase = map toLower
