{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

data BasicMath a where
  Add :: Num a => BasicMath a -> BasicMath a -> BasicMath a
  Multiply :: Num a => BasicMath a -> BasicMath a -> BasicMath a
  Negate :: Num a => BasicMath a -> BasicMath a
  Map :: (a -> b) -> BasicMath a -> BasicMath b
  Term :: a -> BasicMath a

eval :: BasicMath a -> a
eval = \case
  Term a -> a
  Add a b -> (eval a) + (eval b)
  Multiply a b -> (eval a) * (eval b)
  Negate a -> negate (eval a)
  Map f a -> f (eval a)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
