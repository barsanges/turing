{- |
   Module      : Commons.Iterator
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a mathematical puzzle by iterating over all the different views of
the grid.
-}

module Commons.Iterator (
  solveWithIt
  ) where

import Data.Sequence ( Seq(..), (|>) )
import Commons.Solve ( solve )

-- | Cyclic iterator over an object 'u' indexed by 'a's.
data It a u = It (Seq a) u

instance Functor (It a) where
  fmap f (It as x) = It as (f x)

-- | Get the next view from a grid.
nextIt :: (grid -> a -> Maybe view)
       -> It a grid
       -> Maybe (It a view)
nextIt _ (It Empty _) = Nothing
nextIt f (It (a :<| as) g) = case f g a of
  Just v -> Just (It (as |> a) v)
  Nothing -> Nothing

-- | Solve a mathematical puzzle by iterating over all the different views of
-- the grid.
solveWithIt :: (Integral n, Functor t, Foldable t)
            => n                              -- ^ Number of iterations before giving up
            -> grid                           -- ^ Initial puzzle
            -> Seq a                          -- ^ Every view's key
            -> (grid -> a -> Maybe view)      -- ^ Get a part of the puzzle to solve
            -> t (view -> view)               -- ^ Set of rules to use to shrink the grid
            -> (view -> grid)                 -- ^ Get the full puzzle
            -> Maybe grid
solveWithIt limit g0 as next shrinks unview =

  case solve limit (It as g0) next' shrinks' unview' of
    Just (It _ g) -> Just g
    Nothing -> Nothing

  where

    next' = nextIt next
    shrinks' = fmap fmap shrinks
    unview' = fmap unview
