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

module Theme where

import Color
import ColorX11
import Data.Proxy

sampleColorSet =
  instantiateTheme $
  AddColor (namedRGB @"red" @255 @0 @0) $
  AddColor (namedRGB @"green" @0 @255 @0) $
  AddColor AliceBlue $
  AddColor DarkViolet
  NewTheme
