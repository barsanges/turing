{-# LANGUAGE OverloadedStrings #-}

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
  solveFutoshiki,
  solveFutoshikiWithLog,
  processFutoshiki
  ) where

import Data.Either ( lefts, rights )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector ( Vector, (!), (//), generate, indexed )
import Text.Printf ( printf )
import Commons.Cell ( Hole, Cell, hFromSet, hToSet, hSize, hDifference,
                      hDifference', hNotIn, cToChar, cMin, cMax, hLowerBound,
                      hUpperBound )
import Commons.Digit ( Digit(..) )
import Commons.Iterator ( solveWithIt, solveWithLogAndIt )
import Commons.Log ( Message(..), Log, dropLog, describeChange, record,
                     records )
import Commons.Parsing ( parse, unparse )

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

-- | An ASCII template for a Futoshiki grid.
template :: String
template = "\
\-------------\n\
\|_?_?_?_?_?_|\n\
\|! ! ! ! ! !|\n\
\|_?_?_?_?_?_|\n\
\|! ! ! ! ! !|\n\
\|_?_?_?_?_?_|\n\
\|! ! ! ! ! !|\n\
\|_?_?_?_?_?_|\n\
\|! ! ! ! ! !|\n\
\|_?_?_?_?_?_|\n\
\|! ! ! ! ! !|\n\
\|_?_?_?_?_?_|\n\
\-------------\n\
\"

-- | Parse a string into a Futoshiki grid.
fromString :: String -> Maybe Futoshiki
fromString s = do
  vec <- parse template '_' cellFromChar s
  slines <- parse template '?' lineSignFromChar s
  scols <- parse template '!' colSignFromChar s
  return F { grid = vec
           , signs = M.union (lineVecToMap slines) (colVecToMap scols)
           }

  where

    cellFromChar :: Char -> Maybe (Cell Digit)
    cellFromChar '1' = Just (Left One)
    cellFromChar '2' = Just (Left Two)
    cellFromChar '3' = Just (Left Three)
    cellFromChar '4' = Just (Left Four)
    cellFromChar '5' = Just (Left Five)
    cellFromChar '6' = Just (Left Six)
    cellFromChar '_' = Just (Right (hFromSet One Two (S.fromList [Three,
                                                                  Four,
                                                                  Five,
                                                                  Six])))
    cellFromChar _ = Nothing

    lineSignFromChar :: Char -> Maybe (Maybe Sign)
    lineSignFromChar '>' = Just (Just Gt)
    lineSignFromChar '<' = Just (Just Lt)
    lineSignFromChar ' ' = Just (Nothing)
    lineSignFromChar _ = Nothing

    colSignFromChar :: Char -> Maybe (Maybe Sign)
    colSignFromChar 'ˇ' = Just (Just Gt)
    colSignFromChar 'ˆ' = Just (Just Lt)
    colSignFromChar ' ' = Just (Nothing)
    colSignFromChar _ = Nothing

    lineVecToMap :: Vector (Maybe Sign) -> M.Map (Int, Int) Sign
    lineVecToMap v = foldr go M.empty (indexed v)
      where
        go :: (Int, Maybe Sign) -> M.Map (Int, Int) Sign -> M.Map (Int, Int) Sign
        go (_, Nothing) m = m
        go (p, Just sg) m = M.insert (p + k, p + k + 1) sg m
          where
            k = p `div` 5

    colVecToMap :: Vector (Maybe Sign) -> M.Map (Int, Int) Sign
    colVecToMap v = foldr go M.empty (indexed v)
      where
        go :: (Int, Maybe Sign) -> M.Map (Int, Int) Sign -> M.Map (Int, Int) Sign
        go (_, Nothing) m = m
        go (p, Just sg) m = M.insert (p, p + 6) sg m

-- | Turn a Futoshiki grid into a string.
toString :: Futoshiki -> String
toString f = s2
  where
    s0 = unparse template '_' cToChar (grid f)
    s1 = unparse s0 '?' lineSignToChar (lineMapToVec $ signs f)
    s2 = unparse s1 '!' colSignToChar (colMapToVec $ signs f)

    lineSignToChar :: Maybe Sign -> Char
    lineSignToChar (Just Gt) = '>'
    lineSignToChar (Just Lt) = '<'
    lineSignToChar Nothing = ' '

    colSignToChar :: Maybe Sign -> Char
    colSignToChar (Just Gt) = 'ˇ'
    colSignToChar (Just Lt) = 'ˆ'
    colSignToChar Nothing = ' '

    lineMapToVec :: M.Map (Int, Int) Sign -> Vector (Maybe Sign)
    lineMapToVec m = generate 30 go
      where
        go :: Int -> Maybe Sign
        go p = M.lookup (p + k, p + k + 1) m
          where
            k = p `div` 5

    colMapToVec :: M.Map (Int, Int) Sign -> Vector (Maybe Sign)
    colMapToVec m = generate 30 go
      where
        go :: Int -> Maybe Sign
        go p = M.lookup (p, p + 6) m

-- | Get the location ID of a view.
toLocationId :: Int -> Dir -> T.Text
toLocationId i d = T.pack $ printf base i
  where
    base = case d of
      Column -> "%02d/    column"
      Row ->    "%02d/       row"
      Comp _ -> "%02d/comparison"

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
  , ruleId = T.pack $ printf "%10s" rule
  , change = describeChange old new
  }

-- | Indexers for all possible views of a grid.
allViews :: Futoshiki -> [(Int, Dir)]
allViews f = simple ++ comp
  where
    simple = [(i, d) | i <- [0..35]
                     , d <- [Column, Row]]
    comp = let go (i, j) = [(i, Comp j), (j, Comp i)]
           in concatMap go (M.keys (signs f))

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
see v = fmap ((vgrid v) !) (neighbors (focus v) (dir v))

-- | Get all the neighbors (in all directions) of the focus cell.
seeAll :: View -> [Cell Digit]
seeAll v = fmap ((vgrid v) !) (S.toList (allNeighbors (focus v)))

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
comparison :: View -> Log View
comparison v = case x of
  Left _ -> pure v
  Right hole -> case dir v of
    Column -> pure v
    Row -> pure v
    Comp j -> case mnew of
      Just new -> let m = mkMessage v "comparison" x new
                  in record m (v { vgrid = vec // [(i, new)] })
      Nothing -> let m = mkMessage v "comparison" x x
                 in record m v
      where
        y = vec ! j
        mnew = case relation v i j of
                 Gt -> hLowerBound hole (cMin y)
                 Lt -> hUpperBound hole (cMax y)

  where

    i = focus v
    vec = vgrid v
    x = vec ! i

-- | A digit can appear only once in a view.
unique :: View -> Log View
unique v = case vec ! i of
  Left _ -> pure v
  Right hole -> case dir v of
    Comp _ -> pure v
    _ -> case hDifference hole known of
      Nothing -> let m = mkMessage v "unique" x x
                 in record m v
      Just new -> let m = mkMessage v "unique" x new
                  in record m (v { vgrid = vec // [(i, new)] })
  where
    i = focus v
    vec = vgrid v
    x = vec ! i
    known = S.fromList (lefts (see v))

-- | Only one cell may be available for a digit.
only :: View -> Log View
only v = case vec ! i of
  Left _ -> pure v
  Right hole -> case dir v of
    Comp _ -> pure v
    _ -> case hNotIn hole others of
      Nothing -> let m = mkMessage v "only" (vec ! i) (vec ! i)
                 in record m v
      Just x -> if S.notMember x known
                then let m = mkMessage v "only" (vec ! i) (Left x)
                     in record m (v { vgrid = vec // [(i, Left x)] })
                else let m = mkMessage v "only" (vec ! i) (vec ! i)
                     in record m v
  where
    i = focus v
    vec = vgrid v
    known = S.fromList (lefts (seeAll v))
    unknown = rights (see v)
    others = S.unions (fmap hToSet unknown)

-- | Some cells may constitute a subset independant of the other cells of the
-- view.
subset :: View -> Log View
subset v = case vec ! i of
  Left _ -> pure v
  Right hole -> case dir v of
    Comp _ -> pure v
    _ -> if 1 + length (identical hole) == hSize hole
         then records ms (v { vgrid = vec // news })
         else let m = mkMessage v "subset" (vec ! i) (vec ! i)
              in record m v
      where
        news = newNeighbors hole
        ms = [mkMessage' j (dir v) "subset" (vec ! j) new
             | (j, new) <- news ]

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
            then case hDifference' h' h of
                   Nothing -> (j, Right h')
                   Just c -> (j, c)
            else (j, Right h')

-- | Rules to apply to increase the available information on a view.
shrink :: View -> View
shrink = dropLog . shrinkWithLog

-- | Rules to apply to increase the available information on a view. Record
-- the process.
shrinkWithLog :: View -> Log View
shrinkWithLog v = (comparison v) >>= unique >>= only >>= subset

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
solveFutoshiki limit g0 = solveWithIt limit g0 (allViews g0) select shrink unview

-- | Solve a Futoshiki grid, and record how the cells are progressively
-- simplified.
solveFutoshikiWithLog :: Integral n
                      => n               -- ^ Number of iterations before giving up
                      -> Futoshiki       -- ^ Initial grid
                      -> Either (Log Futoshiki) (Log Futoshiki)
solveFutoshikiWithLog limit g0 = solveWithLogAndIt limit g0 (allViews g0) select shrinkWithLog unview

-- | Parse, solve and unparse a Futoshiki puzzle.
processFutoshiki :: Integral n => n -> String -> (String, Maybe String)
processFutoshiki limit input = case fromString input of
  Nothing -> ("unable to parse the input!", Nothing)
  Just f -> case solveFutoshiki limit f of
    Left f' -> ("unable to solve the puzzle!", Just $ toString f')
    Right f' -> ("", Just $ toString f')
