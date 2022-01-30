{- |
   Module      : Games.FutoshikiSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Games.Futoshiki.
-}

module Games.FutoshikiSpec ( spec ) where

import Test.Hspec
import Data.Char ( isSpace )
import Data.Maybe ( fromJust )
import Commons.Log ( dropLog )
import Commons.Solve ( Solution(..) )
import Games.Futoshiki

trimEnd :: String -> String
trimEnd s = reverse (dropWhile isSpace (reverse s))

trimLinesEnd :: String -> String
trimLinesEnd s = unlines (fmap trimEnd (lines s))

solutionToString :: Solution Futoshiki -> String
solutionToString (Impossible _) = ""
solutionToString (Partial x) = trimLinesEnd (toString $ dropLog x)
solutionToString (Solved y) = trimLinesEnd (toString $ dropLog y)

compFromFiles :: FilePath -> FilePath -> Expectation
compFromFiles f1 f2 = do
  initial <- fmap (fromJust . fromString) (readFile f1)
  expected <- fmap trimLinesEnd (readFile f2)
  solutionToString (solveFutoshiki (5000 :: Int) initial) `shouldBe` expected

spec :: Spec
spec = do
  describe "solveFutoshiki" $ do
    it "01" $ compFromFiles "test/Games/futoshikis/01-initial" "test/Games/futoshikis/01-expected"
