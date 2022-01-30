{- |
   Module      : Games.GaramSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Games.Garam.
-}

module Games.GaramSpec ( spec ) where

import Test.Hspec
import Data.Maybe ( fromJust )
import Commons.Log ( dropLog )
import Commons.Solve ( Solution(..) )
import Games.Garam.Solve

solutionToString :: Solution Garam -> String
solutionToString (Impossible _) = ""
solutionToString (Partial x) = toString $ dropLog x
solutionToString (Solved y) = toString $ dropLog y

compFromFiles :: FilePath -> FilePath -> Expectation
compFromFiles f1 f2 = do
  initial <- fmap (fromJust . fromString) (readFile f1)
  expected <- readFile f2
  solutionToString (solveGaram (5000 :: Int) initial) `shouldBe` expected

spec :: Spec
spec = do
  describe "solveGaram" $ do
    it "01" $ compFromFiles "test/Games/garams/01-initial" "test/Games/garams/01-expected"

    it "02" $ compFromFiles "test/Games/garams/02-initial" "test/Games/garams/02-expected"

    it "03" $ compFromFiles "test/Games/garams/03-initial" "test/Games/garams/03-expected"
