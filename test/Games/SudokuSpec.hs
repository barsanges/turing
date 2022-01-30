{- |
   Module      : Games.SudokuSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Games.Sudoku.
-}

module Games.SudokuSpec ( spec ) where

import Test.Hspec
import Data.Maybe ( fromJust )
import Commons.Log ( dropLog )
import Commons.Solve ( Solution(..) )
import Games.Sudoku

solutionToString :: Solution Sudoku -> String
solutionToString (Impossible _) = ""
solutionToString (Partial x) = toString $ dropLog x
solutionToString (Solved y) = toString $ dropLog y

compFromFiles :: FilePath -> FilePath -> Expectation
compFromFiles f1 f2 = do
  initial <- fmap (fromJust . fromString) (readFile f1)
  expected <- readFile f2
  solutionToString (solveSudoku (5000 :: Int) initial) `shouldBe` expected

spec :: Spec
spec = do
  describe "solveSudoku" $ do
    it "01" $ compFromFiles "test/Games/sudokus/01-initial" "test/Games/sudokus/01-expected"

    it "02" $ compFromFiles "test/Games/sudokus/02-initial" "test/Games/sudokus/02-expected"
