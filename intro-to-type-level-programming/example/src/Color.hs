{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Color where
import Data.Bifunctor (second)
import Data.Kind
import Data.List    (find)
import Data.Proxy
import Data.Word    (Word8)
import GHC.TypeLits
import Text.Printf
import qualified Data.Map.Strict as Map

data RGB = RGB
  { rgbRed   :: Word8
  , rgbGreen :: Word8
  , rgbBlue  :: Word8
  } deriving (Eq, Show)

class IsColor a where
  toRGB :: a -> RGB

rgbHex :: IsColor a => a -> String
rgbHex a =
  let (RGB r g b) = toRGB a
  in printf "%02x%02x%02x" r g b

class IsColor a => NamedColor a where
  type ColorName a :: Symbol

colorNameVal :: forall a. KnownSymbol (ColorName a) => String
colorNameVal = symbolVal $ Proxy @(ColorName a)

colorNameVal' :: forall a proxy. KnownSymbol (ColorName a) => a -> String
colorNameVal' _ = symbolVal $ Proxy @(ColorName a)

data RGBColor (r :: Nat) (g :: Nat) (b :: Nat) = RGBColor

instance ValidRGB r g b => IsColor (RGBColor r g b) where
  toRGB _ = RGB (natWord8 @r) (natWord8 @g) (natWord8 @b)

data NamedRGB (name :: Symbol) (r :: Nat) (g :: Nat) (b :: Nat) = NamedRGB

namedRGB :: forall name r g b. (KnownSymbol name, ValidRGB r g b) => NamedRGB name r g b
namedRGB = NamedRGB

instance ValidRGB r g b => IsColor (NamedRGB name r g b) where
  toRGB _ = toRGB $ RGBColor :: RGBColor r g b

type family NatHex (n :: Nat) :: Symbol where
  NatHex 0 = "0"
  NatHex 1 = "1"
  NatHex 2 = "2"
  NatHex 3 = "3"
  NatHex 4 = "4"
  NatHex 5 = "5"
  NatHex 6 = "6"
  NatHex 7 = "7"
  NatHex 8 = "8"
  NatHex 9 = "9"
  NatHex 10 = "A"
  NatHex 11 = "B"
  NatHex 12 = "C"
  NatHex 13 = "D"
  NatHex 14 = "E"
  NatHex 15 = "F"
  NatHex n = NatHex (Div n 16) `AppendSymbol` NatHex (Mod n 16)

type family PadNatHex (n :: Nat) :: Symbol where
  PadNatHex n =
    IfThenElse (n <=? 15) ("0" `AppendSymbol` NatHex n) (NatHex n)

instance IsColor (RGBColor r g b) => NamedColor (RGBColor r g b) where
  type ColorName _ =
    (("#" `AppendSymbol` PadNatHex r)
      `AppendSymbol` PadNatHex g
    ) `AppendSymbol` PadNatHex b

instance IsColor (NamedRGB name r g b) => NamedColor (NamedRGB name r g b) where
  type ColorName _ = name

natWord8 :: forall n . (KnownNat n, n <= 255) => Word8
natWord8 = fromIntegral $ natVal (Proxy @n)

type ValidRGB r g b = (KnownNat r, KnownNat g, KnownNat b, r <= 255, g <= 255, b <= 255)

instance IsColor RGB where
  toRGB = id

data SomeColor = forall color. IsColor color => SomeColor color

someRGB :: Word8 -> Word8 -> Word8 -> SomeColor
someRGB r g b = SomeColor $ RGB r g b

instance IsColor SomeColor where
  toRGB (SomeColor color) = toRGB color

type Theme = [Symbol]

newtype ThemeInstance (a :: Theme) = ThemeInstance { getThemeInstance :: Map.Map String SomeColor }

showThemeInstance :: ThemeInstance (t :: Theme) -> String
showThemeInstance (ThemeInstance t) =
  let
    t' = Map.map toRGB t
  in show t'

type family HasColor (n :: Symbol) (t :: Theme) :: Bool where
  HasColor n '[] = False
  HasColor n (n : _) = True
  HasColor n (n' : rest) = HasColor n rest

type family WithColor (n :: Symbol) (t :: Theme) :: Constraint where
  WithColor colorName '[] = TypeError (Text (AppendSymbol "Missing Color: " colorName)) :: Constraint
  WithColor colorName (colorName : rest) = ()
  WithColor colorName (otherColor : rest) = WithColor colorName rest

type family EQ a b :: Bool where
  EQ a a = True
  EQ a b = False

type family IfThenElse (p :: Bool) (t :: a) (f :: a) where
  IfThenElse True t f = t
  IfThenElse False t f = f

type family FirstMatching (colors :: [Symbol]) (t :: Theme) :: Symbol where
  FirstMatching (color:colors) t =
    IfThenElse (EQ True (HasColor color t)) color (FirstMatching colors t)
  FirstMatching '[] t = TypeError (Text "No candidate color matches")

lookupColor :: forall colorName theme.
  (KnownSymbol colorName, WithColor colorName theme)
  => ThemeInstance theme -> RGB
lookupColor (ThemeInstance colors) =
  let
    targetColorName = symbolVal $ Proxy @colorName
  in toRGB $ colors Map.! targetColorName

colorDemo theme =
  let r = lookupColor @"red" theme
      g = lookupColor @"green" theme
      b = lookupColor @"blue" theme
  in show (r,g,b)

lookupDefault :: forall colorName defaultName theme foundColor.
  ( KnownSymbol colorName
  , KnownSymbol defaultName
  , KnownSymbol foundColor
  , foundColor ~ FirstMatching [colorName, defaultName] theme
  , WithColor foundColor theme
  ) => ThemeInstance theme -> RGB
lookupDefault = lookupColor @foundColor

type family ThemeColors (t :: Theme) :: Symbol where
  ThemeColors '[] = ""
  ThemeColors (color : rest) = ""

data MkTheme theme where
  NewTheme :: MkTheme '[]
  AddColor :: (KnownSymbol (ColorName color), NamedColor color) => color -> MkTheme theme -> MkTheme (ColorName color : theme)

instantiateTheme :: MkTheme theme -> ThemeInstance theme
instantiateTheme mkTheme =
  case mkTheme of
    NewTheme -> ThemeInstance Map.empty
    AddColor color mkTheme' ->
      let
        (ThemeInstance t) = instantiateTheme mkTheme'
        colorName = colorNameVal' color
        colorVal = SomeColor $ toRGB color
      in ThemeInstance $ Map.insert colorName colorVal t

class ValidateThemeInstance (theme :: Theme) (a :: Theme -> Type) where
  validateThemeInstance :: Map.Map String SomeColor -> Either String (a theme)

instance ValidateThemeInstance '[] ThemeInstance where
  validateThemeInstance theme = Right (ThemeInstance theme)

instance ( KnownSymbol currentColor
         , ValidateThemeInstance rest ThemeInstance
         ) => ValidateThemeInstance (currentColor:rest) ThemeInstance where
  validateThemeInstance theme =
    let targetColor = symbolVal $ Proxy @currentColor
    in case Map.lookup targetColor theme of
      Nothing ->
        let colorName = symbolVal $ Proxy @currentColor
        in Left $ "missing color: " <> colorName
      Just _ -> do
        (ThemeInstance m) <- validateThemeInstance @rest theme
        pure $ ThemeInstance m

themeInstance :: forall theme. ValidateThemeInstance theme ThemeInstance => Map.Map String SomeColor -> Either String (ThemeInstance theme)
themeInstance = validateThemeInstance
