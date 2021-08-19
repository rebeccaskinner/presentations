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

data RGBColor = RGBColor
  { rgbRed   :: Word8
  , rgbGreen :: Word8
  , rgbBlue  :: Word8
  } deriving (Eq, Show)

class IsColor a where
  toRGB :: a -> RGBColor

class IsColor a => NamedColor a where
  type ColorName :: a -> Symbol

data RGBColor' (r :: Nat) (g :: Nat) (b :: Nat) = RGBColor'

instance ValidRGB r g b => IsColor (RGBColor' r g b) where
  toRGB = reifyRGBColor

natWord8 :: forall n . (KnownNat n, n <= 255) => Word8
natWord8 = fromIntegral $ natVal (Proxy @n)

type ValidRGB r g b = (KnownNat r, KnownNat g, KnownNat b, r <= 255, g <= 255, b <= 255)

reifyRGBColor
  :: forall r g b. ValidRGB r g b
  => RGBColor' r g b -> RGBColor
reifyRGBColor _proxy = RGBColor (natWord8 @r) (natWord8 @g) (natWord8 @b)

instance IsColor RGBColor where
  toRGB = id

data SomeColor = forall color. IsColor color => SomeColor color

someRGB :: Word8 -> Word8 -> Word8 -> SomeColor
someRGB r g b = SomeColor $ RGBColor r g b

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

data ColorKind a b
  = Color a
  | ColorPair (a,b)

type SatisfiesColor color theme = (True ~ HasColor color theme)

lookupColor :: forall colorName theme.
  (KnownSymbol colorName, SatisfiesColor colorName theme)
  => ThemeInstance theme -> RGBColor
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
  ) => ThemeInstance theme -> RGBColor
lookupDefault = lookupColor @foundColor

sampleColorSet :: ThemeInstance (ColorTheme "red" (ColorTheme "green" EmptyTheme))
sampleColorSet = ThemeInstance $ [("red", someRGB 255 0 0)
                                 ,("green", someRGB 0 255 0)]
