{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module ThemeLoader where

import Color
import ColorX11
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Identity
import Data.Kind
import Data.Map.Strict qualified as Map
import Text.Read

newtype ColorReference r a =
  ColorReference {unColorReference :: ExceptT String (Reader r) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadError String)

evalColorReference :: r -> ColorReference r a -> Either String a
evalColorReference env =
  flip runReader env . runExceptT . unColorReference

data ColorValue w
  = RGBValue RGB
  | X11Value SomeColor
  | OtherColor (HKD w (ColorValue w))

deriving instance Show (ColorValue Identity)

instance IsColor (ColorValue Identity) where
  toRGB (RGBValue rgb) = rgb
  toRGB (X11Value c) = toRGB c
  toRGB (OtherColor ref) = toRGB ref

instance FromJSON (ColorValue (ColorReference RawRuntimeTheme)) where
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
        colors <- asks (getRuntimeTheme . getRawRuntimeTheme)
        case Map.lookup name colors of
          Nothing -> throwError $ "Referenced color not found: " <> name
          Just color' -> pure color'

dereferenceColorValue ::
  RawRuntimeTheme ->
  ColorValue (ColorReference RawRuntimeTheme) ->
  Either String (ColorValue Identity)
dereferenceColorValue env colorValue =
  case colorValue of
    RGBValue r -> pure $ RGBValue r
    X11Value c -> pure $ X11Value c
    OtherColor r -> do
      referencedColor <- dereferenceColorValue env =<< evalColorReference env r
      case referencedColor of
        OtherColor _ -> pure referencedColor
        _ -> pure $ OtherColor referencedColor

type family HKD (wrapper :: a -> a) (value :: a) :: a where
  HKD Identity value = value
  HKD wrapper value = wrapper value

newtype RawRuntimeTheme = RawRuntimeTheme
  {getRawRuntimeTheme :: RuntimeTheme' (ColorReference RawRuntimeTheme)}
  deriving newtype (FromJSON)

newtype RuntimeTheme' (w :: Type -> Type) = RuntimeTheme'
  {getRuntimeTheme :: Map.Map String (ColorValue w)}

instance FromJSON (RuntimeTheme' (ColorReference RawRuntimeTheme)) where
  parseJSON = fmap RuntimeTheme' . parseJSON

type RuntimeTheme = RuntimeTheme' Identity

deriving instance Show RuntimeTheme

evalConfig :: RawRuntimeTheme -> Either String RuntimeTheme
evalConfig rawConfig =
  fmap RuntimeTheme'
    . traverse (dereferenceColorValue rawConfig)
    . getRuntimeTheme
    . getRawRuntimeTheme
    $ rawConfig

loadRuntimeTheme :: FilePath -> IO RuntimeTheme
loadRuntimeTheme p = do
  contents <- BS.readFile p
  case eitherDecode' contents >>= evalConfig of
    Left err -> ioError $ userError err
    Right val -> pure val

parseRGB :: MonadFail m => String -> m RGB
parseRGB s =
  case s of
    '#' : s' -> parseRGB s'
    [r, r', g, g', b, b'] -> do
      RGB
        <$> parseHex r r'
        <*> parseHex g g'
        <*> parseHex b b'
    _ ->
      fail $
        "invalid RGB hex string "
          <> s
          <> " expected a string in the form of #1a2b3c"
  where
    parseHex a b =
      case readEither ['0', 'x', a, b] of
        Left err -> fail err
        Right val -> pure val

parseX11Color :: MonadFail m => String -> m SomeColor
parseX11Color colorName =
  let (ThemeInstance t) = x11Theme
   in case Map.lookup colorName t of
        Nothing -> fail $ "no x11 color " <> colorName
        Just color -> pure color

rgbTheme :: RuntimeTheme -> Map.Map String SomeColor
rgbTheme = Map.map SomeColor . getRuntimeTheme

type SampleTheme = '["red", "green", "blue", "text", "border"]

sampleColorSet :: ThemeInstance '["red", "green", "blue"]
sampleColorSet =
  instantiateTheme $
    AddColor (RenameColor @"red" DarkViolet) $
      AddColor (namedRGB @"green" @0 @255 @0) $
        AddColor (RenameColor @"blue" RebeccaPurple) $
          NewTheme

sampleThemer ::
  ( WithColor "red" theme
  , WithColor "green" theme
  , WithColor "blue" theme
  ) =>
  ThemeInstance theme ->
  String
sampleThemer theme =
  show
    ( lookupColor @"red" theme
    , lookupColor @"green" theme
    , lookupColor @"blue" theme
    )

sampleQuery ::
  ( WithColor "blue" theme
  , WithColor "green" theme
  , WithColor "red" theme
  ) =>
  ThemeInstance theme ->
  String
sampleQuery themeInstance =
  let r = lookupColor @"red" themeInstance
      g = lookupColor @"green" themeInstance
      b = lookupColor @"blue" themeInstance
   in show (r, g, b)

validateThemeConfig ::
  forall (theme :: Theme).
  ValidateThemeInstance theme ThemeInstance =>
  RuntimeTheme ->
  Either String (ThemeInstance theme)
validateThemeConfig =
  validateThemeInstance . Map.map SomeColor . getRuntimeTheme

type RuntimeTestTheme = ["blue", "green", "red"]

testQuery :: FilePath -> IO ()
testQuery p = do
  cfg <- loadRuntimeTheme p
  let testSampleQuery t = (lookupColor @"red" t, lookupColor @"blue" t)
      r = testSampleQuery <$> validateThemeConfig @RuntimeTestTheme cfg
  print r

testMkTheme :: IO ()
testMkTheme = do
  print $ sampleQuery sampleColorSet

sampleQuery' ::
  ( WithColor "value" theme
  , WithColor "saturation" theme
  , WithColor "hue" theme
  ) =>
  ThemeInstance theme ->
  String
sampleQuery' themeInstance =
  let h = lookupColor @"hue" themeInstance
      s = lookupColor @"saturation" themeInstance
      v = lookupColor @"value" themeInstance
   in show (h, s, v)
