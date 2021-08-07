{- |
   Module      : Commons.Cell
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Type representing a cell in a grid, whose value can either be known or unknown.
-}

module Commons.Cell (
  Hole,
  fromSet,
  toSet,
  Cell,
  size,
  difference,
  difference',
  notIn,
  toChar
  ) where

import qualified Data.Set as S
import Commons.Digit ( Digit(..) )

-- | A cell in a grid, whose value is unknown -i.e.: at least two different
-- values are possible.
data Hole a = Hole a a (S.Set a)
  deriving Show

instance (Eq a, Ord a) => Eq (Hole a) where
  h == h' = (toSet h) == (toSet h')

-- | A cell in a grid, whose value can either be known or unknown.
type Cell a = Either a (Hole a)

-- | The number of elements in the hole.
size :: Hole a -> Int
size (Hole _ _ s) = 2 + S.size s

-- | Build a hole from a set.
fromSet :: a -> a -> S.Set a -> Hole a
fromSet x y s = Hole x y s

-- | Turn a hole into a set.
toSet :: Ord a => Hole a -> S.Set a
toSet (Hole x y s) = S.insert x (S.insert y s)

-- | Difference of a hole and a set: return elements of the hole not existing in
-- the set. Return 'Nothing' when the difference is empty.
difference :: Ord a => Hole a -> S.Set a -> Maybe (Cell a)
difference h s = case S.toList (S.difference (toSet h) s) of
  [] -> Nothing
  [x] -> Just (Left x)
  (x1:x2:xs) -> Just (Right (Hole x1 x2 (S.fromList xs)))

-- | Difference of two holes: return elements of the first hole not existing in
-- the second one. Return 'Nothing' when the difference is empty.
difference' :: Ord a => Hole a -> Hole a -> Maybe (Cell a)
difference' h h' = difference h (toSet h')

-- | Return an element from the hole that is not in the set.
notIn :: Ord a => Hole a -> S.Set a -> Maybe a
notIn h s = foldr go Nothing (toSet h)
  where
    go x Nothing = if S.notMember x s
                   then Just x
                   else Nothing
    go _ (Just y) = Just y

-- | Turn a cell containing a digit into a character.
toChar :: Cell Digit -> Char
toChar (Left One) = '1'
toChar (Left Two) = '2'
toChar (Left Three) = '3'
toChar (Left Four) = '4'
toChar (Left Five) = '5'
toChar (Left Six) = '6'
toChar (Left Seven) = '7'
toChar (Left Eight) = '8'
toChar (Left Nine) = '9'
toChar (Right _) = '_'
