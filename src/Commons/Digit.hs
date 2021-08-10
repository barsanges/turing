{- |
   Module      : Commons.Digit
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Digits from 0 to 9.
-}

module Commons.Digit (
  Digit(..),
  toInt
  ) where

-- | Digit from 1 to 9.
data Digit = One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Zero
  deriving (Eq, Ord)

instance Show Digit where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Zero = "0"

-- | Convert the digit to an int.
toInt :: Digit -> Int
toInt Zero = 0
toInt One = 1
toInt Two = 2
toInt Three = 3
toInt Four = 4
toInt Five = 5
toInt Six = 6
toInt Seven = 7
toInt Eight = 8
toInt Nine = 9
