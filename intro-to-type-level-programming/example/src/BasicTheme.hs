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

class HasColor (color :: Symbol) (container :: Theme)
instance HasColor color (color ': colors)
instance {-# overlappable #-} (HasColor color colors) => HasColor color (currentColor ': colors)

data RGB = RGB
  { rgbRed   :: Word8
  , rgbGreen :: Word8
  , rgbBlue  :: Word8
  } deriving (Eq, Show)

newtype ThemeInstance (theme :: Theme) =
  ThemeInstance { getThemeInstance :: Map.Map String RGB } deriving (Eq, Show)

class ValidateThemeInstance (theme :: Theme) (a :: Theme -> Type) where
  validateThemeInstance :: Map.Map String RGB -> Maybe (a theme)

instance ValidateThemeInstance '[] ThemeInstance where
  validateThemeInstance theme = Just (ThemeInstance theme)

instance ( KnownSymbol currentColor
         , ValidateThemeInstance rest ThemeInstance
         ) => ValidateThemeInstance (currentColor:rest) ThemeInstance where
  validateThemeInstance theme = do
    let targetColor = symbolVal $ Proxy @currentColor
    Map.lookup targetColor theme
    (ThemeInstance m) <- validateThemeInstance @rest theme
    pure $ ThemeInstance m

themeInstance :: ValidateThemeInstance theme ThemeInstance => Map.Map String RGB -> Maybe (ThemeInstance theme)
themeInstance = validateThemeInstance

lookupColor
  :: forall colorName theme.
  ( KnownSymbol colorName
  , HasColor colorName theme)
  => ThemeInstance theme -> RGB
lookupColor (ThemeInstance colors) =
  let
    targetName = symbolVal $ Proxy @colorName
  in colors Map.! targetName

demoThemeInstance :: ThemeInstance ["red","green","blue"]
demoThemeInstance = ThemeInstance . Map.fromList $
  [("red", RGB 0xff 0x00 0x00)]

colorDemo
  :: ( HasColor "red" theme
     , HasColor "green" theme
     , HasColor "blue" theme )
  => ThemeInstance theme -> String
colorDemo theme =
  let r = lookupColor @"red" theme
      g = lookupColor @"green" theme
      b = lookupColor @"blue" theme
  in show (r,g,b)

type DemoTheme = '["red", "green", "blue"]
type YellowTheme = '["yellow"]

-- demoThemeInstance = Map.fromList $
--   [ ("red", RGB 255 0 0)
--   , ("green", RGB 0 255 0)
--   , ("blue", RGB 0 0 255)
--   ]
