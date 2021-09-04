{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Commons.Log
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Log which rules are applied to a grid.
-}

module Commons.Log
  ( Message(..)
  , Log
  , swap
  , (>>*)
  , describeChange
  , record
  , records
  , getLog
  , dropLog
  ) where

import Data.Foldable ( toList )
import Data.Sequence ( Seq(..), (><), singleton, fromList )
import qualified Data.Set as S
import qualified Data.Text as T
import Commons.Cell ( Cell, cToSet )

-- | Record the effect of one rule on the grid.
data Message = Mes { locationId :: T.Text
                   , ruleId :: T.Text
                   , change :: T.Text
                   }
  deriving Show

-- | Log which rules are applied to the grid.
data Log a = Log (Seq Message) a
  deriving Show

instance Functor Log where
  fmap f (Log s x) = Log s (f x)

instance Applicative Log where
  pure x = Log Empty x
  (Log s f) <*> (Log s' x) = Log (s >< s') (f x)

instance Monad Log where
  (Log s x) >>= f = Log (s >< s') x'
    where Log s' x' = f x

-- | Swap the position of the log and the functor in the input's type.
swap :: Functor f => Log (f a) -> f (Log a)
swap (Log s x) = fmap (Log s) x

-- | Apply an operation involving a functor, to a logged value.
(>>*) :: Functor f => Log a -> (a -> f b) -> f (Log b)
lx >>* f = swap (fmap f lx)

-- | Pretty print a set.
prettySet :: Show a => S.Set a -> T.Text
prettySet xs = if S.size xs == 0
               then "{ }"
               else T.concat ["{ ", vals, " }"]
  where
    vals = T.intercalate ", " (fmap (T.pack . show) (S.elems xs))

-- | Describe the difference between two cells, 'old' and 'new'.
describeChange :: (Eq a, Ord a, Show a) => Cell a -> Cell a -> T.Text
describeChange old new =
  if old == new
  then "no change"
  else T.concat ["left: ", prettySet sNew, " removed: ", prettySet diff]
  where
    sOld = cToSet old
    sNew = cToSet new
    diff = S.difference sOld sNew

-- | Log a message with a value.
record :: Message -> a -> Log a
record m x = Log (singleton m) x

-- | Log a list of messages with a value.
records :: [Message] -> a -> Log a
records ms x = Log (fromList ms) x

-- | Turn one message into a string.
toString :: Message -> T.Text
toString m = T.concat ["[", locationId m, "] [", ruleId m, "] ", change m]

-- | Get the log (i.e.: a sequence of messages) as a string.
getLog :: Log a -> T.Text
getLog (Log ms _) = T.unlines (toList $ fmap toString ms)

-- | Drop the log.
dropLog :: Log a -> a
dropLog (Log _ x) = x
