{- |
   Module      : Commons.Solve
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a mathematical puzzle.
-}

module Commons.Solve
  ( Solution(..)
  , solve
  ) where

import Data.Text as T
import Commons.Log ( Log, (>>*), swap, getLog )

-- | The solution to a mathematical puzzle.
data Solution grid = Impossible T.Text
                   | Partial (Log grid)
                   | Solved (Log grid)

-- | Solve a mathematical puzzle, and record which rules are applied to the
-- grid.
solve :: Integral n
      => n                          -- ^ Number of iterations -- before giving up
      -> grid                       -- ^ Initial puzzle
      -> (grid -> Maybe view)       -- ^ Get a part of the puzzle to solve
      -> (view -> Log (Maybe view)) -- ^ Rule to use to shrink the grid
      -> (view -> grid)             -- ^ Get the full puzzle
      -> Solution grid
solve limit g0 next shrink unview = go 0 (pure g0)

  where

    go n g = case g >>* next of
      Nothing -> Solved g
      Just v -> if n > limit
        then Partial g
        else case swap lv' of
               Just v' -> go (n + 1) (fmap unview v')
               Nothing -> Impossible (getLog lv')
        where
          lv' = (v >>= shrink)
