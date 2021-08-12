{- |
   Module      : Commons.Cell
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Type representing a cell in a grid, whose value can either be known or unknown.
-}

module Commons.Cell (
  Hole,
  hFromSet,
  hToSet,
  Cell,
  cToSet,
  hSize,
  hDifference,
  hDifference',
  hNotIn,
  cToChar,
  cMin,
  cMax,
  cFilter,
  hFilter,
  hLowerBound,
  hUpperBound
  ) where

import qualified Data.Set as S
import Commons.Digit ( Digit(..) )

-- | A cell in a grid, whose value is unknown -i.e.: at least two different
-- values are possible.
data Hole a = Hole a a (S.Set a)
  deriving Show

instance (Eq a, Ord a) => Eq (Hole a) where
  h == h' = (hToSet h) == (hToSet h')

-- | A cell in a grid, whose value can either be known or unknown.
type Cell a = Either a (Hole a)

-- | The number of elements in the hole.
hSize :: Hole a -> Int
hSize (Hole _ _ s) = 2 + S.size s

-- | Build a hole from a set.
hFromSet :: a -> a -> S.Set a -> Hole a
hFromSet = Hole

-- | Turn a cell into a set.
cToSet :: Ord a => Cell a -> S.Set a
cToSet (Left x) = S.singleton x
cToSet (Right h) = hToSet h

-- | Turn a hole into a set.
hToSet :: Ord a => Hole a -> S.Set a
hToSet (Hole x y s) = S.insert x (S.insert y s)

-- | Difference of a hole and a set: return elements of the hole not existing in
-- the set. Return 'Nothing' when the difference is empty.
hDifference :: Ord a => Hole a -> S.Set a -> Maybe (Cell a)
hDifference h s = case S.toList (S.difference (hToSet h) s) of
  [] -> Nothing
  [x] -> Just (Left x)
  (x1:x2:xs) -> Just (Right (Hole x1 x2 (S.fromList xs)))

-- | Difference of two holes: return elements of the first hole not existing in
-- the second one. Return 'Nothing' when the difference is empty.
hDifference' :: Ord a => Hole a -> Hole a -> Maybe (Cell a)
hDifference' h h' = hDifference h (hToSet h')

-- | Return an element from the hole that is not in the set.
hNotIn :: Ord a => Hole a -> S.Set a -> Maybe a
hNotIn h s = foldr go Nothing (hToSet h)
  where
    go x Nothing = if S.notMember x s
                   then Just x
                   else Nothing
    go _ (Just y) = Just y

-- | Return the minimal possible value in the cell.
cMin :: Ord a => Cell a -> a
cMin (Left x) = x
cMin (Right h) = minimum (hToSet h)

-- | Return the maximal possible value in the cell.
cMax :: Ord a => Cell a -> a
cMax (Left x) = x
cMax (Right h) = maximum (hToSet h)

-- | Filter all elements that satisfy the predicate if the cell is a hole. If
-- the cell is an unique value, 'cFilter f == id'.
cFilter :: Ord a => (a -> Bool) -> Cell a -> Cell a
cFilter _ (Left x) = Left x
cFilter f (Right h) = case hFilter f h of
  Nothing -> Right h
  Just c -> c

-- | Filter all elements that satisfy the predicate.
hFilter :: Ord a => (a -> Bool) -> Hole a -> Maybe (Cell a)
hFilter f h = case S.toList (S.filter f (hToSet h)) of
  [] -> Nothing
  [x] -> Just (Left x)
  (x1:x2:xs) -> Just (Right (Hole x1 x2 (S.fromList xs)))

-- | Remove all possible values lower than a given value.
hLowerBound :: Ord a => Hole a -> a -> Maybe (Cell a)
hLowerBound h x = hFilter (\ y -> y > x) h

-- | Remove all possible values higher than a given value.
hUpperBound :: Ord a => Hole a -> a -> Maybe (Cell a)
hUpperBound h x = hFilter (\ y -> y < x) h

-- | Turn a cell containing a digit into a character.
cToChar :: Cell Digit -> Char
cToChar (Left Zero) = '0'
cToChar (Left One) = '1'
cToChar (Left Two) = '2'
cToChar (Left Three) = '3'
cToChar (Left Four) = '4'
cToChar (Left Five) = '5'
cToChar (Left Six) = '6'
cToChar (Left Seven) = '7'
cToChar (Left Eight) = '8'
cToChar (Left Nine) = '9'
cToChar (Right _) = '_'
