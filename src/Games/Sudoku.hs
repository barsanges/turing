{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Games.Sudoku
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a Sudoku puzzle: fill a 9x9 grid with digits so that each column,
each row, and each of the nine 3x3 blocks that compose the grid
contains all of the digits from 1 to 9.
-}

module Games.Sudoku (
  Sudoku,
  fromString,
  toString,
  solveSudoku,
  processSudoku
  ) where

import Data.Either ( lefts, rights )
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector ( Vector, (!), (//) )
import Text.Printf ( printf )
import Commons.Cell ( Hole, Cell, hFromSet, hToSet, hSize, hDifference,
                      hDifference', hNotIn, cToChar )
import Commons.Digit ( Digit(..) )
import Commons.Iterator ( solveWithIt )
import Commons.Log ( Message(..), Log, describeChange, record, records )
import Commons.Parsing ( parse, unparse )

-- | A Sudoku grid.
newtype Sudoku = Sk (Vector (Cell Digit))
  deriving Show

-- | A view of the grid, focused on a specific cell within a specific area.
data View = View { focus :: Int
                 , dir :: Dir
                 , grid :: Vector (Cell Digit)
                 }
  deriving Show

-- | The "direction" of a view of the grid.
data Dir = Column
         | Row
         | Block
  deriving Show

-- | An ASCII template for a Sudoku grid.
template :: String
template = "\
\-------------\n\
\|___|___|___|\n\
\|___|___|___|\n\
\|___|___|___|\n\
\-------------\n\
\|___|___|___|\n\
\|___|___|___|\n\
\|___|___|___|\n\
\-------------\n\
\|___|___|___|\n\
\|___|___|___|\n\
\|___|___|___|\n\
\-------------\n\
\"

-- | Parse a string into a Sudoku grid.
fromString :: String -> Maybe Sudoku
fromString s = fmap Sk (parse template '_' fromChar s)

  where

    fromChar :: Char -> Maybe (Cell Digit)
    fromChar '1' = Just (Left One)
    fromChar '2' = Just (Left Two)
    fromChar '3' = Just (Left Three)
    fromChar '4' = Just (Left Four)
    fromChar '5' = Just (Left Five)
    fromChar '6' = Just (Left Six)
    fromChar '7' = Just (Left Seven)
    fromChar '8' = Just (Left Eight)
    fromChar '9' = Just (Left Nine)
    fromChar '_' = Just (Right (hFromSet One Two (S.fromList [Three,
                                                              Four,
                                                              Five,
                                                              Six,
                                                              Seven,
                                                              Eight,
                                                              Nine])))
    fromChar _ = Nothing

-- | Turn a Sudoku grid into a string.
toString :: Sudoku -> String
toString (Sk vec) = unparse template '_' cToChar vec

-- | Get the location ID of a view.
toLocationId :: Int -> Dir -> T.Text
toLocationId i d = T.pack $ printf base i
  where
    base = case d of
      Column -> "%02d/column"
      Row ->    "%02d/   row"
      Block ->  "%02d/ block"

-- | Build a message regarding the update of a given view.
mkMessage :: View
          -> T.Text
          -> Cell Digit
          -> Cell Digit
          -> Message
mkMessage v rule old new = mkMessage' (focus v) (dir v) rule old new

-- | Build a message regarding the update of a given cell.
mkMessage' :: Int
           -> Dir
           -> T.Text
           -> Cell Digit
           -> Cell Digit
           -> Message
mkMessage' i d rule old new = Mes
  { locationId = toLocationId i d
  , ruleId = T.pack $ printf "%6s" rule
  , change = describeChange old new
  }

-- | Indexers for all possible views of a grid.
allViews :: [(Int, Dir)]
allViews = [(i, d) | i <- [0..80]
                   , d <- [Column, Row, Block]]

-- | Get the indices of all cells in either the same row, the same column or
-- the same block than the given cell.
neighbors :: Int -> Dir -> [Int]
neighbors i d = case d of
  Column -> [p * 9 + col | p <- [0..8]
                         , p /= row]
  Row -> [row * 9 + q | q <- [0..8]
                      , q /= col]
  Block -> [p * 9 + q | p <- [(3 * br)..(3 * br + 2)]
                      , q <- [(3 * bc)..(3 * bc + 2)]
                      , p /= row || q /= col]
  where
    row = i `div` 9
    br = row `div` 3
    col = i `mod` 9
    bc = col `div` 3

-- | Get the indices of all cells in the same row, column or block than the
-- given cell.
allNeighbors :: Int -> S.Set Int
allNeighbors i = S.unions [col, row, block]
  where
    col = S.fromList (neighbors i Column)
    row = S.fromList (neighbors i Row)
    block = S.fromList (neighbors i Block)

-- | Select a view of a grid.
select :: Sudoku -> (Int, Dir) -> Maybe View
select (Sk vec) (i, d) = case vec ! i of
  Left _ -> Nothing -- The cell is already known.
  Right _ -> Just (View { focus = i
                        , dir = d
                        , grid = vec
                        })

-- | Get all the neighbors (in the current direction) of the focus cell.
see :: View -> [Cell Digit]
see v = fmap ((grid v) !) (neighbors (focus v) (dir v))

-- | Get all the neighbors (in all directions) of the focus cell.
seeAll :: View -> [Cell Digit]
seeAll v = fmap ((grid v) !) (S.toList (allNeighbors (focus v)))

-- | A digit can appear only once in a view.
unique :: View -> Log View
unique v = case x of
  Left _ -> pure v
  Right hole -> case hDifference hole known of
    Nothing -> let m = mkMessage v "unique" x x
               in record m v
    Just new -> let m = mkMessage v "unique" x new
                in record m v { grid = vec // [(i, new)] }
  where
    i = focus v
    vec = grid v
    x =vec ! i
    known = S.fromList (lefts (see v))

-- | Only one cell may be available for a digit.
only :: View -> Log View
only v = case x of
  Left _ -> pure v
  Right hole -> case hNotIn hole others of
    Nothing -> let m = mkMessage v "only" x x
               in record m v
    Just y -> if S.notMember y known
              then let m = mkMessage v "only" x (Left y)
                   in record m v { grid = vec // [(i, Left y)] }
              else let m = mkMessage v "only" x x
                   in record m v
  where
    i = focus v
    vec = grid v
    x = vec ! i
    known = S.fromList (lefts (seeAll v))
    unknown = rights (see v)
    others = S.unions (fmap hToSet unknown)

-- | Some cells may constitute a subset independant of the other cells of the
-- view.
subset :: View -> Log View
subset v = case x of
  Left _ -> pure v
  Right hole -> if 1 + length (identical hole) == hSize hole
    then records ms (v { grid = vec // news })
    else let m = mkMessage v "subset" x x
         in record m v
    where
      news = newNeighbors hole
      ms = [mkMessage' j (dir v) "subset" (vec ! j) new
           | (j, new) <- news]

  where

    i = focus v
    ngb = neighbors (focus v) (dir v)
    vec = grid v
    x = vec ! i
    unknown = rights (see v)

    identical :: Hole Digit -> [Hole Digit]
    identical h = filter (\ h' -> h' == h) unknown

    newNeighbors :: Hole Digit -> [(Int, Cell Digit)]
    newNeighbors h = fmap go ngb
      where
        go :: Int -> (Int, Cell Digit)
        go j = case vec ! j of
          Left y -> (j, Left y)
          Right h' -> if h' /= h
            then case hDifference' h' h of
                   Nothing -> (j, Right h')
                   Just c -> (j, c)
            else (j, Right h')

-- | Rules to apply to increase the available information on a view. Record
-- the process.
shrink :: View -> Log View
shrink v = (unique v) >>= only >>= subset

-- | Get the grid behind the given view.
unview :: View -> Sudoku
unview v = Sk (grid v)

-- | Solve a Sudoku grid, and record how the cells are progressively simplified.
solveSudoku :: Integral n
            => n            -- ^ Number of iterations before giving up
            -> Sudoku       -- ^ Initial grid
            -> Either (Log Sudoku) (Log Sudoku)
solveSudoku limit g0 = solveWithIt limit g0 allViews select shrink unview

-- | Parse, solve and unparse a Sudoku puzzle.
processSudoku :: Integral n => n -> String -> (String, Maybe (Log String))
processSudoku limit input = case fromString input of
  Nothing -> ("unable to parse the input!", Nothing)
  Just s -> case solveSudoku limit s of
    Left s' -> ("unable to solve the puzzle!", Just $ fmap toString s')
    Right s' -> ("", Just $ fmap toString s')
