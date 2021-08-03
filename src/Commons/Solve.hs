{- |
   Module      : Commons.Solve
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a mathematical puzzle.
-}

module Commons.Solve (
  solve
  ) where

-- | Solve a mathematical puzzle.
solve :: (Integral n, Foldable t)
      => n                         -- ^ Number of iterations before giving up
      -> grid                      -- ^ Initial puzzle
      -> (grid -> Maybe view)      -- ^ Get a part of the puzzle to solve
      -> t (view -> view)          -- ^ Set of rules to use to shrink the grid
      -> (view -> grid)            -- ^ Get the full puzzle
      -> Maybe grid
solve limit g0 next shrinks unview = go 0 g0

  where

    go n g = case next g of
      Nothing -> Just g
      Just v -> if n > limit
        then Nothing
        else go (n + 1) (unview $ foldr ($) v shrinks)
