{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Theme where

import Color
import ColorX11
import Data.Proxy

-- type SampleTheme = '["red", "green", "blue", "text", "border"]

sampleTheme = instantiateTheme $ sampleColorSet

sampleColorSet =
  AddColor (namedRGB @"red" @255 @0 @0) $
  AddColor (namedRGB @"green" @0 @255 @0) $
  AddColor RebeccaPurple $
  AddColor DarkViolet
  NewTheme
