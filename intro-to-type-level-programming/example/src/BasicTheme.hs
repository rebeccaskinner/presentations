{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module BasicTheme where
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Data.Kind
import qualified Data.Map.Strict as Map

type Theme = [Symbol]
class HasColor (color :: Symbol) (container :: [Symbol])
instance HasColor color (color ': colors)
instance (HasColor color colors) => HasColor color (currentColor ': colors)

data RGB = RGB
  { rgbRed   :: Word8
  , rgbGreen :: Word8
  , rgbBlue  :: Word8
  } deriving (Eq, Show)

newtype ThemeInstance (theme :: Theme) =
  ThemeInstance { getThemeInstance :: Map.Map String RGB } deriving (Eq, Show)

lookupColor :: forall colorName theme. (KnownSymbol colorName, HasColor colorName theme) => ThemeInstance theme -> RGB
lookupColor (ThemeInstance colors) =
  let
    targetName = symbolVal $ Proxy @colorName
  in colors Map.! targetName

colorDemo theme =
  let r = lookupColor @"red" theme
      g = lookupColor @"green" theme
      b = lookupColor @"blue" theme
  in show (r,g,b)

type DemoTheme = '["red", "green", "blue"]

runDemo :: IO ()
runDemo =
  let themeInstance = ThemeInstance $
