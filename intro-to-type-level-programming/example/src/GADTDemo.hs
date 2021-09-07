{-# LANGUAGE GADTs #-}
module GADTDemo where

data SomeValue = SomeInt Int | SomeString String | SomeBool Bool

notSomeValue :: SomeValue -> SomeValue
notSomeValue val =
  case val of
    SomeInt _ -> val
    SomeString _ -> val
    SomeBool b -> SomeBool (not b)

data SomeValue' where
  SomeInt' :: Num a => a -> SomeValue'
  SomeString' :: String -> SomeValue'
  SomeBool' :: Bool -> SomeValue'

data Weird t = NotSoWeird | Weird (Weird Int)

-- data SomeValue t where
--   SomeInt :: Int -> SomeValue Int
--   SomeString :: String -> SomeValue String
--   SomeBool :: Bool -> SomeValue Bool

-- notSomeValue :: SomeValue Bool -> SomeValue Bool
-- notSomeValue (SomeBool b) = SomeBool (not b)

-- someStringLengthGADT :: SomeValue String -> SomeValue Int
-- someStringLengthGADT (SomeString s) = SomeInt (length s)
