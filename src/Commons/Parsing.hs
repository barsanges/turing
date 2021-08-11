{- |
   Module      : Commons.Parsing
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Parse (parts of) a puzzle from a string.
-}

module Commons.Parsing (
    parse
  , unparse
  ) where

import Data.Char ( isSpace )
import Data.Vector ( Vector, fromList, (!) )

-- | Remove trailing whitespaces at the end of each line.
trimLinesEnd :: String -> String
trimLinesEnd str = unlines (fmap trimEnd (lines str))
  where
    trimEnd :: String -> String
    trimEnd s = reverse (dropWhile isSpace (reverse s))

-- | Remove empty lines.
rmEmpty :: String -> String
rmEmpty str = unlines (filter (\ s -> s /= []) (lines str))

-- | Remove trailing whitespaces and empty lines.
sanitize :: String -> String
sanitize = rmEmpty . trimLinesEnd

-- | Parse a part of a puzzle from a string.
parse :: String -> Char -> (Char -> Maybe a) -> String -> Maybe (Vector a)
parse template mark conv str =
  if (length template) == (length str')
  then fmap fromList (go (zip template str'))
  else Nothing

  where

    str' = sanitize str

    go [] = Just []
    go ((x, y):zs) =
      if x == mark
      then do
        v <- conv y
        vs <- go zs
        return (v:vs)
      else go zs


-- | Convert a part of a puzzle to a string.
unparse :: String -> Char -> (a -> Char) -> Vector a -> String
unparse template mark conv vec = go template 0
  where
    go [] _ = []
    go (c:cs) i =
      if c == mark
      then (conv (vec ! i):go cs (i+1))
      else (c:go cs i)
