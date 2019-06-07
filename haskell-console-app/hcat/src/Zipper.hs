module Zipper where
import qualified Data.Maybe as Maybe

data Zipper a = Zipper ![a] ![a] deriving Show

mkZipper :: [a] -> Zipper a
mkZipper = flip Zipper []

isEnd, isStart, isLast :: Zipper a -> Bool
isEnd (Zipper [] end) = True
isEnd (Zipper [lastElem] end) = True
isEnd _ = False
isLast (Zipper [last] _) = True
isLast _ = False
isStart (Zipper start []) = True
isStart _ = False

start, end, last, next, prev :: Zipper a -> Zipper a
next z@(Zipper [] _) = z
next (Zipper (cur:rest) end) = Zipper rest (cur:end)
prev z@(Zipper _ []) = z
prev (Zipper start (last:end)) = Zipper (last:start) end
last = prev . end
end (Zipper start end) = Zipper [] (reverse start ++ end)
start (Zipper start end) = Zipper ((reverse end) ++ start) []

get :: Zipper a -> Maybe a
get (Zipper cur _) = Maybe.listToMaybe cur

length :: Zipper a -> Int
length (Zipper a b) = Prelude.length a + Prelude.length b

idx :: Zipper a -> Int
idx (Zipper _ a) = Prelude.length a
