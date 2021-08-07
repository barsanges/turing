{- |
   Module      : Games.SudokuSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Games.Sudoku.
-}

module Games.SudokuSpec ( spec ) where

import Test.Hspec
import Data.Maybe ( fromJust )
import Games.Sudoku

eitherToString :: Either Sudoku Sudoku -> String
eitherToString (Left x) = toString x
eitherToString (Right y) = toString y

compFromFiles :: FilePath -> FilePath -> Expectation
compFromFiles f1 f2 = do
  initial <- fmap (fromJust . fromString) (readFile f1)
  expected <- readFile f2
  eitherToString (solveSudoku (5000 :: Int) initial) `shouldBe` expected

spec :: Spec
spec = do
  describe "solveSudoku" $ do
    it "01" $ compFromFiles "test/Games/sudokus/01-initial" "test/Games/sudokus/01-expected"

    it "02" $ compFromFiles "test/Games/sudokus/02-initial" "test/Games/sudokus/02-expected"
