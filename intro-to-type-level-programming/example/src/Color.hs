{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Color where
import Data.Word (Word8)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

data RGB = RGB
  { rgbRed   :: Word8
  , rgbGreen :: Word8
  , rgbBlue  :: Word8
  } deriving (Eq, Show)

class IsColor a where
  toRGB :: a -> RGB

class IsColor a => NamedColor a where
  type ColorName a :: Symbol

colorNameVal :: forall a name. (KnownSymbol name, name ~ ColorName a) => NamedColor a => String
colorNameVal = symbolVal $ Proxy @(ColorName a)

colorNameVal' :: forall a name. (KnownSymbol name, name ~ ColorName a) => NamedColor a => a -> String
colorNameVal' _ = symbolVal $ Proxy @(ColorName a)

data RGBColor (r :: Nat) (g :: Nat) (b :: Nat) = RGBColor

instance ValidRGB r g b => IsColor (RGBColor r g b) where
  toRGB = reifyRGBColor

data NamedRGB (name :: Symbol) (r :: Nat) (g :: Nat) (b :: Nat) = NamedRGB

instance ValidRGB r g b => IsColor (NamedRGB name r g b) where
  toRGB _ = reifyRGBColor (RGBColor :: RGBColor r g b)

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
      (("#" `AppendSymbol` PadNatHex r) `AppendSymbol` PadNatHex g) `AppendSymbol` PadNatHex b

instance IsColor (NamedRGB name r g b) => NamedColor (NamedRGB name r g b) where
  type ColorName _ = name

natWord8 :: forall n . (KnownNat n, n <= 255) => Word8
natWord8 = fromIntegral $ natVal (Proxy @n)

type ValidRGB r g b = (KnownNat r, KnownNat g, KnownNat b, r <= 255, g <= 255, b <= 255)

reifyRGBColor
  :: forall r g b. ValidRGB r g b
  => RGBColor r g b -> RGB
reifyRGBColor _proxy = RGB (natWord8 @r) (natWord8 @g) (natWord8 @b)

instance IsColor RGB where
  toRGB = id

data SomeColor = forall color. IsColor color => SomeColor color

someRGB :: Word8 -> Word8 -> Word8 -> SomeColor
someRGB r g b = SomeColor $ RGB r g b

instance IsColor SomeColor where
  toRGB (SomeColor color) = toRGB color

data Theme
  = EmptyTheme
  | ColorTheme Symbol Theme

newtype ThemeInstance (a :: Theme) = ThemeInstance { getThemeInstance :: [(String, SomeColor)] }

type family MergeThemes (a :: Theme) (b :: Theme) where
  MergeThemes a EmptyTheme = a
  MergeThemes a (ColorTheme color rest) =
    MergeThemes (ColorTheme color a) rest

type family HasColor (n :: Symbol) (t :: Theme) :: Bool where
  HasColor n EmptyTheme = False
  HasColor n (ColorTheme n _) = True
  HasColor n (ColorTheme n' rest) = HasColor n rest

type family EQ a b :: Bool where
  EQ a a = True
  EQ a b = False

type family IfThenElse (cond :: Bool) (trueBranch :: a) (falseBranch :: a) where
  IfThenElse True t f = t
  IfThenElse False t f = f

type family ThemeSatisfies (n :: Symbol) (t :: Theme) :: Constraint where
  ThemeSatisfies color theme =
    IfThenElse (EQ True (HasColor color theme))
    (() :: Constraint)
    (TypeError (Text "Missing Color") :: Constraint)

type family FirstMatching (colors :: [Symbol]) (t :: Theme) :: Symbol where
  FirstMatching (color:colors) t =
    IfThenElse (EQ True (HasColor color t)) color (FirstMatching colors t)
  FirstMatching '[] t = TypeError (Text "No candidate color matches")

type SatisfiesColor color theme = (True ~ HasColor color theme)

lookupColor :: forall colorName theme.
  (KnownSymbol colorName, SatisfiesColor colorName theme)
  => ThemeInstance theme -> RGB
lookupColor (ThemeInstance colors) =
  let
    targetColorName = symbolVal $ Proxy @colorName
  in go targetColorName colors
  where
    go name ((name',val):colors)
      | name == name' = toRGB val
      | otherwise = go name colors

lookupDefault :: forall colorName defaultName theme foundColor.
  ( KnownSymbol colorName
  , KnownSymbol defaultName
  , KnownSymbol foundColor
  , foundColor ~ FirstMatching [colorName, defaultName] theme
  , SatisfiesColor foundColor theme
  ) => ThemeInstance theme -> RGB
lookupDefault = lookupColor @foundColor

sampleColorSet :: ThemeInstance (ColorTheme "red" (ColorTheme "green" EmptyTheme))
sampleColorSet = ThemeInstance $ [("red", someRGB 255 0 0)
                                 ,("green", someRGB 0 255 0)]

data MkTheme theme where
  NewTheme :: MkTheme EmptyTheme
  AddColor :: (KnownSymbol (ColorName color), NamedColor color) => color -> MkTheme theme -> MkTheme (ColorTheme (ColorName color) theme)

instantiateTheme :: MkTheme theme -> ThemeInstance theme
instantiateTheme mkTheme =
  case mkTheme of
    NewTheme -> ThemeInstance []
    AddColor color mkTheme' ->
      let
        (ThemeInstance t) = instantiateTheme mkTheme'
        colorName = colorNameVal' color
        colorVal = SomeColor $ toRGB color
      in ThemeInstance $ (colorName, colorVal) : t
