{- |
   Module      : Games.Futoshiki
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a Futoshiki puzzle: fill a 6x6 grid with digits so that:
  1. Each column and each row contains all of the digits from 1 to 6.
  2. All order relationships given in the grid are enforced.
-}

module Games.Futoshiki (
  Futoshiki,
  fromString,
  toString,
  solveFutoshiki
  ) where

import Data.Either ( lefts, rights )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector ( Vector, (!), (//), fromList )
import Commons.Cell ( Hole, Cell, fromSet, toSet, size, difference, difference',
                      notIn, toChar, cellMin, cellMax, lowerBound, upperBound )
import Commons.Digit ( Digit(..) )
import Commons.Iterator ( solveWithIt )

-- | A comparison sign: > (Gt) or < (Lt).
data Sign = Gt | Lt
  deriving Show

-- | A Futoshiki grid.
data Futoshiki = F { grid :: Vector (Cell Digit)
                   , signs :: M.Map (Int, Int) Sign
                   }
  deriving Show

-- | A view of the grid, focused on a specific cell within a specific area.
data View = View { focus :: Int
                 , dir :: Dir
                 , vgrid :: Vector (Cell Digit)
                 , vsigns :: M.Map (Int, Int) Sign
                 }
  deriving Show

-- | The "direction" of a view of the grid.
data Dir = Column
         | Row
         | Comp Int
  deriving Show

-- | Parse a string into a Futoshiki grid.
fromString :: String -> Maybe Futoshiki
fromString s = case consume s 0 0 ([], M.empty) of
  Just (xs, sgns) -> if length xs == 36
                     then Just (F { grid = fromList (reverse xs)
                                  , signs = sgns
                                  })
                     else Nothing
  Nothing -> Nothing

-- | Consume a string to produce a Futoshiki grid.
consume :: [Char]
        -> Int
        -> Int
        -> ([Cell Digit], M.Map (Int, Int) Sign)
        -> Maybe ([Cell Digit], M.Map (Int, Int) Sign)
consume [] _ _ res = Just res
consume ('\n':cs) row _ tmp = consume cs (row + 1) 0 tmp
consume (c:cs) row col (xs, sgn) = do
  xs' <- parseCell
  let sgn' = parseSign
  consume cs row (col + 1) (xs', sgn')

  where

    parseCell :: Maybe [Cell Digit]
    parseCell = if even row && even col
                then fmap (\ x -> x:xs) (cellFromChar c)
                else Just xs

    cellFromChar :: Char -> Maybe (Cell Digit)
    cellFromChar '1' = Just (Left One)
    cellFromChar '2' = Just (Left Two)
    cellFromChar '3' = Just (Left Three)
    cellFromChar '4' = Just (Left Four)
    cellFromChar '5' = Just (Left Five)
    cellFromChar '6' = Just (Left Six)
    cellFromChar '_' = Just (Right (fromSet One Two (S.fromList [Three,
                                                                 Four,
                                                                 Five,
                                                                 Six])))
    cellFromChar _ = Nothing

    parseSign :: (M.Map (Int, Int) Sign)
    parseSign
      | even row && odd col =
        let i = 6 * (row `div` 2) + (col `div` 2)
            j = i + 1
        in maybeInsert (i, j) (signFromChar c)
      | odd row && even col =
        let i = 6 * (row `div` 2) + (col `div` 2)
            j = i + 6
        in maybeInsert (i, j) (signFromChar c)
      | otherwise = sgn

    signFromChar :: Char -> Maybe Sign
    signFromChar 'ˇ' = Just Gt
    signFromChar 'ˆ' = Just Lt
    signFromChar '>' = Just Gt
    signFromChar '<' = Just Lt
    signFromChar _ = Nothing

    maybeInsert :: (Int, Int) -> Maybe Sign -> M.Map (Int, Int) Sign
    maybeInsert _ Nothing = sgn
    maybeInsert k (Just s) = M.insert k s sgn

-- | Turn a Futoshiki grid into a string.
toString :: Futoshiki -> String
toString f =
  unlines
  [
    [d  0, h  0  1, d  1, h  1  2, d  2, h  2  3, d  3, h  3  4, d  4, h  4  5, d  5]
  , [v  0 6,   ' ', v  1 7,   ' ', v  2 8,   ' ', v  3 9,   ' ', v  4 10,   ' ', v 5 11]
  , [d  6, h  6  7, d  7, h  7  8, d  8, h  8  9, d  9, h  9 10, d 10, h 10 11, d 11]
  , [v  6 12,  ' ', v  7 13,  ' ', v  8 14,  ' ', v  9 15,  ' ', v 10 16,  ' ', v 11 17]
  , [d 12, h 12 13, d 13, h 13 14, d 14, h 14 15, d 15, h 15 16, d 16, h 16 17, d 17]
  , [v 12 18,  ' ', v 13 19,  ' ', v 14 20,  ' ', v 15 21,  ' ', v 16 22,  ' ', v 17 23]
  , [d 18, h 18 19, d 19, h 19 20, d 20, h 20 21, d 21, h 21 22, d 22, h 22 23, d 23]
  , [v 18 24,  ' ', v 19 25,  ' ', v 20 26,  ' ', v 21 27,  ' ', v 22 28,  ' ', v 23 29]
  , [d 24, h 24 25, d 25, h 25 26, d 26, h 26 27, d 27, h 27 28, d 28, h 28 29, d 29]
  , [v 24 30,  ' ', v 25 31,  ' ', v 26 32,  ' ', v 27 33,  ' ', v 28 34,  ' ', v 29 35]
  , [d 30, h 30 31, d 31, h 31 32, d 32, h 32 33, d 33, h 33 34, d 34, h 34 35, d 35]
  ]

  where

    d :: Int -> Char
    d i = toChar ((grid f) ! i)

    h :: Int -> Int -> Char
    h i j = case M.lookup (i, j) (signs f) of
      Nothing -> ' '
      Just Gt -> '>'
      Just Lt -> '<'

    v :: Int -> Int -> Char
    v i j = case M.lookup (i, j) (signs f) of
      Nothing -> ' '
      Just Gt -> 'ˇ'
      Just Lt -> 'ˆ'

-- | Indexers for all possible views of a grid.
allViews :: Futoshiki -> [(Int, Dir)]
allViews f = simple ++ comp
  where
    simple = [(i, d) | i <- [0..35]
                     , d <- [Column, Row]]
    comp = let go (i, j) = [(i, Comp j), (j, Comp i)]
           in concat (fmap go (M.keys (signs f)))

-- | Select a view of a grid.
select :: Futoshiki -> (Int, Dir) -> Maybe View
select f (i, d) = case (grid f) ! i of
  Left _ -> Nothing -- The cell is already known.
  Right _ -> Just View { focus = i
                       , dir = d
                       , vgrid = grid f
                       , vsigns = signs f
                       }

-- | Get the indices of all cells in either the same row, the same column or
-- the same comparison than the given cell.
neighbors :: Int -> Dir -> [Int]
neighbors i d = case d of
  Column -> [p * 6 + col | p <- [0..5]
                         , p /= row]
  Row -> [row * 6 + q | q <- [0..5]
                      , q /= col]
  Comp j -> [j]
  where
    row = i `div` 6
    col = i `mod` 6

-- | Get the indices of all cells in the same row or column than the given cell.
allNeighbors :: Int -> S.Set Int
allNeighbors i = S.unions [col, row]
  where
    col = S.fromList (neighbors i Column)
    row = S.fromList (neighbors i Row)

-- | Get all the neighbors (in the current direction) of the focus cell.
see :: View -> [Cell Digit]
see v = fmap ((!) (vgrid v)) (neighbors (focus v) (dir v))

-- | Get all the neighbors (in all directions) of the focus cell.
seeAll :: View -> [Cell Digit]
seeAll v = fmap ((!) (vgrid v)) (S.toList (allNeighbors (focus v)))

-- | Return the order of the relation between 'i' and 'j', from the point of
-- view of 'i': 'Gt' means that the value in cell 'i' should be greater than
-- in cell 'j'.
relation :: View -> Int -> Int -> Sign
relation v i j = if i < j
                 then (vsigns v) M.! (i, j)
                 else case (vsigns v) M.! (j, i) of
                        Gt -> Lt
                        Lt -> Gt

-- | Some cells are bound by an order relationship.
comparison :: View -> View
comparison v = case vec ! i of
  Left _ -> v
  Right hole -> case dir v of
    Column -> v
    Row -> v
    Comp j -> case mnew of
      Just new -> v { vgrid = vec // [(i, new)] }
      Nothing -> v
      where
        y = vec ! j
        mnew = case relation v i j of
                 Gt -> lowerBound hole (cellMin y)
                 Lt -> upperBound hole (cellMax y)

  where

    i = focus v
    vec = vgrid v

-- | A digit can appear only once in a view.
unique :: View -> View
unique v = case vec ! i of
  Left _ -> v
  Right hole -> case dir v of
    Comp _ -> v
    _ -> case difference hole known of
      Nothing -> v
      Just new -> v { vgrid = vec // [(i, new)] }
  where
    i = focus v
    vec = vgrid v
    known = S.fromList (lefts (see v))

-- | Only one cell may be available for a digit.
only :: View -> View
only v = case vec ! i of
  Left _ -> v
  Right hole -> case dir v of
    Comp _ -> v
    _ -> case notIn hole others of
      Nothing -> v
      Just x -> if S.notMember x known
                then v { vgrid = vec // [(i, Left x)] }
                else v
  where
    i = focus v
    vec = vgrid v
    known = S.fromList (lefts (seeAll v))
    unknown = rights (see v)
    others = S.unions (fmap toSet unknown)

-- | Some cells may constitute a subset independant of the other cells of the
-- view.
subset :: View -> View
subset v = case vec ! i of
  Left _ -> v
  Right hole -> case dir v of
    Comp _ -> v
    _ -> if 1 + length (identical hole) == size hole
         then v { vgrid = vec // (newNeighbors hole) }
         else v

  where

    i = focus v
    ngb = neighbors (focus v) (dir v)
    vec = vgrid v
    unknown = rights (see v)

    identical :: Hole Digit -> [Hole Digit]
    identical h = filter (\ h' -> h' == h) unknown

    newNeighbors :: Hole Digit -> [(Int, Cell Digit)]
    newNeighbors h = fmap go ngb
      where
        go :: Int -> (Int, Cell Digit)
        go j = case vec ! j of
          Left x -> (j, Left x)
          Right h' -> if h' /= h
            then case difference' h' h of
                   Nothing -> (j, Right h')
                   Just c -> (j, c)
            else (j, Right h')

-- | Rules to apply to increase the available information on a view.
rules :: [View -> View]
rules = [comparison, unique, only, subset]

-- | Get the grid behind the given view.
unview :: View -> Futoshiki
unview v = F { grid = vgrid v
             , signs = vsigns v
             }

-- | Solve a Futoshiki grid.
solveFutoshiki :: Integral n
               => n               -- ^ Number of iterations before giving up
               -> Futoshiki       -- ^ Initial grid
               -> Either Futoshiki Futoshiki
solveFutoshiki limit g0 = solveWithIt limit g0 (allViews g0) select rules unview
