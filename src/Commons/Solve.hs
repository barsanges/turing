{- |
   Module      : Commons.Solve
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a mathematical puzzle.
-}

module Commons.Solve
  ( solve
  , solveWithLog
  ) where

import Commons.Log ( Log, (>>*) )

-- | Solve a mathematical puzzle.
solve :: Integral n
      => n                         -- ^ Number of iterations before giving up
      -> grid                      -- ^ Initial puzzle
      -> (grid -> Maybe view)      -- ^ Get a part of the puzzle to solve
      -> (view -> view)            -- ^ Rule to use to shrink the grid
      -> (view -> grid)            -- ^ Get the full puzzle
      -> Either grid grid
solve limit g0 next shrink unview = go 0 g0

  where

    go n g = case next g of
      Nothing -> Right g
      Just v -> if n > limit
        then Left g
        else go (n + 1) (unview $ shrink v)

-- | Solve a mathematical puzzle, and record which rules are applied to the
-- grid.
solveWithLog :: Integral n
             => n                         -- ^ Number of iterations before giving up
             -> grid                      -- ^ Initial puzzle
             -> (grid -> Maybe view)      -- ^ Get a part of the puzzle to solve
             -> (view -> Log view)        -- ^ Rule to use to shrink the grid
             -> (view -> grid)            -- ^ Get the full puzzle
             -> Either (Log grid) (Log grid)
solveWithLog limit g0 next shrink unview = go 0 (pure g0)

  where

    go n g = case g >>* next of
      Nothing -> Right g
      Just v -> if n > limit
        then Left g
        else go (n + 1) (fmap unview $ (v >>= shrink))
