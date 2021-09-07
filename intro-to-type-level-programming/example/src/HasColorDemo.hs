{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module HasColorDemo where
import GHC.TypeLits

type Theme = [Symbol]

class HasColor (color :: Symbol) (theme :: Theme)
instance HasColor color (color:colors)
instance {-# OVERLAPPABLE #-}
  HasColor color colors => HasColor color (color':colors)

testFunction :: HasColor color theme => ()
testFunction = ()
