{- |
   Module      : Commons.Iterator
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a mathematical puzzle by iterating over all the different views of
the grid.
-}

module Commons.Iterator
  ( solveWithIt
  ) where

import Data.Sequence ( Seq(..), (|>), fromList )
import Commons.Log ( Log, swap )
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
  Nothing -> nextIt f (It as g)

-- | Solve a mathematical puzzle by iterating over all the different views of
-- the grid, and record which rules are applied to the grid.
solveWithIt :: Integral n
            => n                              -- ^ Number of iterations before giving up
            -> grid                           -- ^ Initial puzzle
            -> [a]                            -- ^ Every view's key
            -> (grid -> a -> Maybe view)      -- ^ Get a part of the puzzle to solve
            -> (view -> Log view)             -- ^ Set of rules to use to shrink the grid
            -> (view -> grid)                 -- ^ Get the full puzzle
            -> Either (Log grid) (Log grid)
solveWithIt limit g0 as next shrink unview =

  case solve limit (It (fromList as) g0) next' shrink' unview' of
    Left lit -> let (It _ g) = swap lit
                in Left g
    Right lit -> let (It _ g) = swap lit
                 in Right g

  where

    next' = nextIt next
    shrink' (It as' g) = fmap (It as') (shrink g)
    unview' = fmap unview
