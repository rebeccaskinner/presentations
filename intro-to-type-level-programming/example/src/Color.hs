{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Color where

import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Word (Word8)
import GHC.TypeLits
import Text.Printf

infixr 6 :++:
type a :++: b = AppendSymbol a b

data RGB = RGB
  { rgbRed :: Word8
  , rgbGreen :: Word8
  , rgbBlue :: Word8
  }
  deriving (Eq, Show)

class IsColor a where
  toRGB :: a -> RGB

rgbHex :: IsColor a => a -> String
rgbHex a =
  let (RGB r g b) = toRGB a
   in printf "%02x%02x%02x" r g b

class IsColor a => NamedColor a where
  type ColorName a :: Symbol

colorNameVal :: forall a. KnownSymbol (ColorName a) => a -> String
colorNameVal _ = symbolVal $ Proxy @(ColorName a)

data RGBTypeLevel (r :: Nat) (g :: Nat) (b :: Nat) = RGBTypeLevel

class GetColor a where getColor :: String

instance ValidRGB r g b => GetColor (RGBTypeLevel r g b) where
  getColor =
    show $ (natVal $ Proxy @r, natVal $ Proxy @g, natVal $ Proxy @b)

instance ValidRGB r g b => IsColor (RGBTypeLevel r g b) where
  toRGB = const $ RGB (natWord8 @r) (natWord8 @g) (natWord8 @b)
    where
      natWord8 :: forall n. (KnownNat n, n <= 255) => Word8
      natWord8 = fromIntegral $ natVal (Proxy @n)

data NamedRGB (name :: Symbol) (r :: Nat) (g :: Nat) (b :: Nat) = NamedRGB

namedRGB :: forall name r g b. (KnownSymbol name, ValidRGB r g b) => NamedRGB name r g b
namedRGB = NamedRGB

instance ValidRGB r g b => IsColor (NamedRGB name r g b) where
  toRGB _ = toRGB (RGBTypeLevel :: RGBTypeLevel r g b)

data RenameColor (name :: Symbol) =
  forall color. IsColor color => RenameColor color

instance IsColor (RenameColor name) where
  toRGB (RenameColor c) = toRGB c

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
  NatHex n = NatHex (Div n 16) :++: NatHex (Mod n 16)

type family PadNatHex (n :: Nat) :: Symbol where
  PadNatHex n =
    IfThenElse (n <=? 15) ("0" :++: NatHex n) (NatHex n)

instance IsColor (RGBTypeLevel r g b) => NamedColor (RGBTypeLevel r g b) where
  type
    ColorName _ =
    "#" :++: PadNatHex r :++: PadNatHex g :++: PadNatHex b

instance IsColor (NamedRGB name r g b) => NamedColor (NamedRGB name r g b) where
  type ColorName _ = name

instance NamedColor (RenameColor name) where
  type ColorName _ = name

type ValidRGB r g b =
  ( KnownNat r, r <= 255
  , KnownNat g, g <= 255
  , KnownNat b, b <= 255)

instance IsColor RGB where
  toRGB = id

data SomeColor = forall color. IsColor color => SomeColor color

instance Show SomeColor where
  show = show . toRGB

someRGB :: Word8 -> Word8 -> Word8 -> SomeColor
someRGB r g b = SomeColor $ RGB r g b

instance IsColor SomeColor where
  toRGB (SomeColor color) = toRGB color

type Theme = [Symbol]

newtype ThemeInstance (a :: Theme) = ThemeInstance
  {getThemeInstance :: Map.Map String SomeColor}
  deriving (Show)


showThemeInstance :: ThemeInstance (t :: Theme) -> String
showThemeInstance (ThemeInstance t) =
  let t' = Map.map toRGB t
   in show t'

type family HasColor (n :: Symbol) (t :: Theme) :: Bool where
  HasColor n '[] = 'False
  HasColor n (n : _) = 'True
  HasColor n (n' : rest) = HasColor n rest

type family WithColor (n :: Symbol) (t :: Theme) :: Constraint where
  WithColor colorName '[] = TypeError ('Text (AppendSymbol "Missing Color: " colorName)) :: Constraint
  WithColor colorName (colorName : rest) = ()
  WithColor colorName (otherColor : rest) = WithColor colorName rest

type family EQ a b :: Bool where
  EQ a a = 'True
  EQ a b = 'False

type family IfThenElse (p :: Bool) (t :: a) (f :: a) :: a where
  IfThenElse 'True t f = t
  IfThenElse 'False t f = f

type family FirstMatching (colors :: [Symbol]) (t :: Theme) :: Symbol where
  FirstMatching (color : colors) t =
    IfThenElse (EQ 'True (HasColor color t)) color (FirstMatching colors t)
  FirstMatching '[] t = TypeError ('Text "No candidate color matches")

lookupColor ::
  forall colorName theme.
  (KnownSymbol colorName, WithColor colorName theme) =>
  ThemeInstance theme ->
  RGB
lookupColor (ThemeInstance colors) =
  let targetColorName = symbolVal $ Proxy @colorName
   in toRGB $ colors Map.! targetColorName

colorDemo
  :: (WithColor "blue" theme, WithColor "green" theme,
      WithColor "red" theme) =>
     ThemeInstance theme -> String
colorDemo theme =
  let r = lookupColor @"red" theme
      g = lookupColor @"green" theme
      b = lookupColor @"blue" theme
   in show (r, g, b)

lookupDefault ::
  forall colorName defaultName theme foundColor.
  ( KnownSymbol colorName
  , KnownSymbol defaultName
  , KnownSymbol foundColor
  , foundColor ~ FirstMatching [colorName, defaultName] theme
  , WithColor foundColor theme
  ) =>
  ThemeInstance theme ->
  RGB
lookupDefault = lookupColor @foundColor

type family ThemeColors (t :: Theme) :: Symbol where
  ThemeColors '[] = ""
  ThemeColors (color : rest) = ""

data MkTheme theme where
  NewTheme :: MkTheme '[]
  AddColor :: (KnownSymbol (ColorName color), NamedColor color) => color -> MkTheme theme -> MkTheme (ColorName color : theme)

data MakeSimpleTheme (theme :: Theme) where
  SimpleNewTheme :: MakeSimpleTheme '[]
  SimpleAddColor ::
    (KnownSymbol colorName) =>
    SomeColor ->
    MakeSimpleTheme theme ->
    MakeSimpleTheme (colorName : theme)

class ToThemeInstance (a :: Theme -> Type) (t :: Theme) where
  toThemeInstance :: a t -> ThemeInstance t

instance ToThemeInstance MakeSimpleTheme '[] where
  toThemeInstance SimpleNewTheme = ThemeInstance Map.empty

instance
  (ToThemeInstance MakeSimpleTheme colors) =>
  ToThemeInstance MakeSimpleTheme (color : colors)
  where
  toThemeInstance (SimpleAddColor rgb rest) =
    let (ThemeInstance m) = toThemeInstance rest
        colorName = symbolVal $ Proxy @color
     in ThemeInstance (Map.insert colorName rgb m)

simpleDemo :: MakeSimpleTheme '["red", "green", "blue"]
simpleDemo =
  SimpleAddColor @"red"
    (someRGB 255 0 0)
    ( SimpleAddColor @"green"
        (someRGB 0 255 0)
        (SimpleAddColor @"blue" (someRGB 0 0 255) SimpleNewTheme)
    )

instantiateTheme :: MkTheme theme -> ThemeInstance theme
instantiateTheme NewTheme = ThemeInstance Map.empty
instantiateTheme (AddColor color mkTheme') =
  let (ThemeInstance t) = instantiateTheme mkTheme'
      colorName = colorNameVal color
      colorVal = SomeColor $ toRGB color
   in ThemeInstance $ Map.insert colorName colorVal t

class ValidateThemeInstance (theme :: Theme) (a :: Theme -> Type) where
  validateThemeInstance :: Map.Map String SomeColor -> Either String (a theme)

instance ValidateThemeInstance '[] ThemeInstance where
  validateThemeInstance theme = Right (ThemeInstance theme)

instance
  ( KnownSymbol currentColor
  , ValidateThemeInstance rest ThemeInstance
  ) =>
  ValidateThemeInstance (currentColor : rest) ThemeInstance
  where
  validateThemeInstance theme =
    let targetColor = symbolVal $ Proxy @currentColor
     in case Map.lookup targetColor theme of
          Nothing ->
            let colorName = symbolVal $ Proxy @currentColor
             in Left $ "missing color: " <> colorName
          Just _ -> do
            (ThemeInstance m) <- validateThemeInstance @rest theme
            pure $ ThemeInstance m
