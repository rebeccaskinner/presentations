module Zipper where
import qualified Data.Maybe as Maybe

data Zipper a = Zipper ![a] ![a] deriving Show

mkZipper :: [a] -> Zipper a
mkZipper = flip Zipper []

isEnd, isStart :: Zipper a -> Bool
isEnd (Zipper [] end) = True
isEnd _ = False
isStart (Zipper start []) = True
isStart _ = False

next, prev :: Zipper a -> Zipper a
next z@(Zipper [] _) = z
next (Zipper (cur:rest) end) = Zipper rest (cur:end)
prev z@(Zipper _ []) = z
prev (Zipper start (last:end)) = Zipper (last:start) end

get :: Zipper a -> Maybe a
get (Zipper cur _) = Maybe.listToMaybe cur
