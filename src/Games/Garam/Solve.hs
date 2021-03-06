{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Games.Garam.Solve
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve a Garam puzzle: fill a grid so that each operation is correct.
-}

module Games.Garam.Solve (
  Garam,
  Op(..),
  fromString,
  toString,
  fromElements,
  getValues,
  setValue,
  solveGaram,
  processGaram
  ) where

import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector ( Vector, (!), (//), generate, fromList )
import Text.Printf ( printf )
import Commons.Digit ( Digit(..), toInt )
import Commons.Cell ( Cell, cToSet, cToChar, cFilter, hFromSet )
import Commons.Iterator ( solveWithIt )
import Commons.Log ( Message(..), Log, describeChange, record )
import Commons.Parsing ( parse, unparse )
import Commons.Solve ( Solution(..) )

-- | Shorthand for 'Cell Digit'.
type Cell' = Cell Digit

-- | A Garam grid.
data Garam = G { grid :: Vector Cell'
               , operators :: Vector Op
               }
  deriving (Show, Eq)

-- | A view of the grid, focused on a specific cell within a specific area.
data View = V { focus :: Equation Idx1 Idx2
              , vgrid :: Vector Cell'
              , voperators :: Vector Op
              }

-- | An operator between two digits.
data Op = Plus
        | Minus
        | Mul
  deriving (Show, Eq)

-- | An indexer for an expression which right hand side is one digit long.
data Idx1 = Idx1 Int Int Int Int
  deriving Show

-- | An indexer for an expression which right hand side is two digits long.
data Idx2 = Idx2 Int Int Int Int Int
  deriving Show

-- | An expression which right hand side is one digit long.
data Expr1 = Expr1 Cell' Op Cell' Cell'
  deriving Show

-- | An expression which right hand side is two digits long.
data Expr2 = Expr2 Cell' Op Cell' Cell' Cell'
  deriving Show

-- | An equation with an unknown.
data Equation a b = FstLHS1 a
                  | SndLHS1 a
                  | RHS1 a
                  | FstLHS2 b
                  | SndLHS2 b
                  | FstRHS2 b
                  | SndRHS2 b
  deriving Show

-- | An ASCII template for a Garam grid.
template :: String
template = "\
\_?_=_   _?_=_\n\
\?   ?   ?   ?\n\
\_   _?_=_   _\n\
\=   =   =   =\n\
\_   _   _   _\n\
\_?_=_   _?_=_\n\
\  ?       ?\n\
\  _       _\n\
\  =       =\n\
\_?_=_   _?_=_\n\
\?   ?   ?   ?\n\
\_   _?_=_   _\n\
\=   =   =   =\n\
\_   _   _   _\n\
\_?_=_   _?_=_\n\
\"

-- | Parse a string into a Garam grid.
fromString :: String -> Maybe Garam
fromString s = do
  vec <- parse template '_' digitFromChar s
  ops <- parse template '?' opFromChar s
  return (G { grid = vec, operators = ops })

  where

    digitFromChar :: Char -> Maybe (Cell Digit)
    digitFromChar '0' = Just (Left Zero)
    digitFromChar '1' = Just (Left One)
    digitFromChar '2' = Just (Left Two)
    digitFromChar '3' = Just (Left Three)
    digitFromChar '4' = Just (Left Four)
    digitFromChar '5' = Just (Left Five)
    digitFromChar '6' = Just (Left Six)
    digitFromChar '7' = Just (Left Seven)
    digitFromChar '8' = Just (Left Eight)
    digitFromChar '9' = Just (Left Nine)
    digitFromChar '_' = Just (Right (hFromSet Zero One (S.fromList [Two,
                                                                    Three,
                                                                    Four,
                                                                    Five,
                                                                    Six,
                                                                    Seven,
                                                                    Eight,
                                                                    Nine])))
    digitFromChar _ = Nothing

    opFromChar :: Char -> Maybe Op
    opFromChar '+' = Just Plus
    opFromChar '-' = Just Minus
    opFromChar 'x' = Just Mul
    opFromChar _ = Nothing

-- | Turn a Garam grid into a string.
toString :: Garam -> String
toString f = res
  where
    partial = unparse template '_' cToChar (grid f)
    res = unparse partial '?' opToChar (operators f)

    opToChar :: Op -> Char
    opToChar Plus = '+'
    opToChar Minus = '-'
    opToChar Mul = 'x'

-- | Create a Garam grid from a vector of operators and a map of the known
-- values. The grid may be incorrect or impossible to solve.
fromElements :: Vector Op -> IM.IntMap Digit -> Maybe Garam
fromElements ops vals
  | length ops /= 20 = Nothing
  | any (\ n -> n < 0 || n > 43) (IM.keys vals) = Nothing
  | otherwise = Just $ G { grid = generate 44 go, operators = ops }
  where
    go :: Int -> Cell'
    go n = case IM.lookup n vals of
      Just x -> Left x
      Nothing -> Right (hFromSet Zero One (S.fromList [Two,
                                                       Three,
                                                       Four,
                                                       Five,
                                                       Six,
                                                       Seven,
                                                       Eight,
                                                       Nine]))

-- | Get the values of the cells in the grid.
getValues :: Garam -> Vector (Vector Digit)
getValues g = fmap toVec (grid g)
  where
    toVec = fromList . S.toList . cToSet

-- | Set the value of a cell.
setValue :: Int -> Digit -> Garam -> Garam
setValue n x g = g { grid = (grid g) // [(n, Left x)] }

-- | Turn the "index" of a view into a string.
toLocationId :: Equation Idx1 Idx2 -> T.Text
toLocationId (FstLHS1 (Idx1 i _ _ _)) = T.pack $ printf "%02d/LHS1/1" i
toLocationId (SndLHS1 (Idx1 _ i _ _)) = T.pack $ printf "%02d/LHS1/2" i
toLocationId (RHS1 (Idx1 _ _ _ i)) = T.pack $ printf "%02d/RHS1/1" i
toLocationId (FstLHS2 (Idx2 i _ _ _ _)) = T.pack $ printf "%02d/LHS2/1" i
toLocationId (SndLHS2 (Idx2 _ i _ _ _)) = T.pack $ printf "%02d/LHS2/2" i
toLocationId (FstRHS2 (Idx2 _ _ _ i _)) = T.pack $ printf "%02d/RHS2/1" i
toLocationId (SndRHS2 (Idx2 _ _ _ _ i)) = T.pack $ printf "%02d/RHS2/2" i

-- | Indexers for all possible views of a grid.
allViews :: [Equation Idx1 Idx2]
allViews = eq1 ++ eq2
  where
    idx1 = [ Idx1 0 0 1 2
           , Idx1 3 1 4 5
           , Idx1 7 6 8 9
           , Idx1 15 7 16 17
           , Idx1 18 8 19 20
           , Idx1 16 9 21 24
           , Idx1 19 10 22 27
           , Idx1 23 11 24 25
           , Idx1 26 12 27 28
           , Idx1 30 17 31 32
           , Idx1 38 18 39 40
           , Idx1 41 19 42 43]
    idx2 = [ Idx2 0 2 6 11 15
           , Idx2 2 3 7 12 17
           , Idx2 3 4 9 13 18
           , Idx2 5 5 10 14 20
           , Idx2 23 13 29 34 38
           , Idx2 25 14 30 35 40
           , Idx2 26 15 32 36 41
           , Idx2 28 16 33 37 43]
    eq1 = [ctor i | ctor <- [FstLHS1, SndLHS1, RHS1]
                  , i <- idx1]
    eq2 = [ctor i | ctor <- [FstLHS2, SndLHS2, FstRHS2, SndRHS2]
                  , i <- idx2]

-- | Select a view of a grid.
select :: Garam -> Equation Idx1 Idx2 -> Maybe View
select g idx = case unknown eq of
    Left _ -> Nothing
    Right _ -> Just $ V { focus = idx
                        , vgrid = grid g
                        , voperators = operators g
                        }
    where
      eq = fromIdx g idx

-- | Get the equation corresponding to an indexer.
fromIdx :: Garam -> Equation Idx1 Idx2 -> Equation Expr1 Expr2
fromIdx g idx = case idx of
  (FstLHS1 (Idx1 x op y z)) -> FstLHS1 (Expr1 (vec ! x) (ops ! op) (vec ! y) (vec ! z))
  (SndLHS1 (Idx1 x op y z)) -> SndLHS1 (Expr1 (vec ! x) (ops ! op) (vec ! y) (vec ! z))
  (RHS1 (Idx1 x op y z)) -> RHS1 (Expr1 (vec ! x) (ops ! op) (vec ! y) (vec ! z))
  (FstLHS2 (Idx2 x op y z1 z2)) -> FstLHS2 (Expr2 (vec ! x) (ops ! op) (vec ! y) (vec ! z1) (vec ! z2))
  (SndLHS2 (Idx2 x op y z1 z2)) -> SndLHS2 (Expr2 (vec ! x) (ops ! op) (vec ! y) (vec ! z1) (vec ! z2))
  (FstRHS2 (Idx2 x op y z1 z2)) -> FstRHS2 (Expr2 (vec ! x) (ops ! op) (vec ! y) (vec ! z1) (vec ! z2))
  (SndRHS2 (Idx2 x op y z1 z2)) -> SndRHS2 (Expr2 (vec ! x) (ops ! op) (vec ! y) (vec ! z1) (vec ! z2))
  where
    vec = grid g
    ops = operators g

-- | Is the index relevant for the given equation?
elemEq :: Int -> Equation Idx1 Idx2 -> Bool
elemEq i (FstLHS1 (Idx1 x _ y z)) = i `elem` [x, y, z]
elemEq i (SndLHS1 (Idx1 x _ y z)) = i `elem` [x, y, z]
elemEq i (RHS1 (Idx1 x _ y z)) = i `elem` [x, y, z]
elemEq i (FstLHS2 (Idx2 x _ y z1 z2)) = i `elem` [x, y, z1, z2]
elemEq i (SndLHS2 (Idx2 x _ y z1 z2)) = i `elem` [x, y, z1, z2]
elemEq i (FstRHS2 (Idx2 x _ y z1 z2)) = i `elem` [x, y, z1, z2]
elemEq i (SndRHS2 (Idx2 x _ y z1 z2)) = i `elem` [x, y, z1, z2]

-- | Get the index of the unknown in an equation.
unknownIdx :: Equation Idx1 Idx2 -> Int
unknownIdx (FstLHS1 (Idx1 x _ _ _)) = x
unknownIdx (SndLHS1 (Idx1 _ _ y _)) = y
unknownIdx (RHS1 (Idx1 _ _ _ z)) = z
unknownIdx (FstLHS2 (Idx2 x _ _ _ _)) = x
unknownIdx (SndLHS2 (Idx2 _ _ y _ _)) = y
unknownIdx (FstRHS2 (Idx2 _ _ _ z1 _)) = z1
unknownIdx (SndRHS2 (Idx2 _ _ _ _ z2)) = z2

-- | Get the unknown in an equation.
unknown :: Equation Expr1 Expr2 -> Cell'
unknown (FstLHS1 (Expr1 x _ _ _)) = x
unknown (SndLHS1 (Expr1 _ _ y _)) = y
unknown (RHS1 (Expr1 _ _ _ z)) = z
unknown (FstLHS2 (Expr2 x _ _ _ _)) = x
unknown (SndLHS2 (Expr2 _ _ y _ _)) = y
unknown (FstRHS2 (Expr2 _ _ _ z1 _)) = z1
unknown (SndRHS2 (Expr2 _ _ _ _ z2)) = z2

-- | Compute an operation between two digits.
compute :: Digit -> Op -> Digit -> Int
compute x Plus y = (toInt x) + (toInt y)
compute x Minus y = (toInt x) - (toInt y)
compute x Mul y = (toInt x) * (toInt y)

-- | Compute an operation which result should be one digit long.
apply1 :: Digit -> Op -> Digit -> Maybe Digit
apply1 x op y = if res > -1 && res < 10
                then Just (partialFromInt res)
                else Nothing
  where
    res = compute x op y

-- | Compute an operation which result should be two digits long.
apply2 :: Digit -> Op -> Digit -> Maybe (Digit, Digit)
apply2 x op y = if res > 9 && res < 100
                then Just (partialFromInt z1, partialFromInt z2)
                else Nothing
  where
    res = compute x op y
    z1 = res `div` 10
    z2 = res `mod` 10

-- | Convert an int to a digit. The function is partial: an incorrect entry will
-- throw an error.
partialFromInt :: Int -> Digit
partialFromInt 0 = Zero
partialFromInt 1 = One
partialFromInt 2 = Two
partialFromInt 3 = Three
partialFromInt 4 = Four
partialFromInt 5 = Five
partialFromInt 6 = Six
partialFromInt 7 = Seven
partialFromInt 8 = Eight
partialFromInt 9 = Nine
partialFromInt _ = error "incorrect entry" -- Should not happen

-- | Test if the given digit is a licit value for the unknown.
validate :: Equation Expr1 Expr2 -> Digit -> Bool
validate (FstLHS1 (Expr1 _ op y z)) x = hasCorrectCombination1 (S.singleton x) op (cToSet y) (cToSet z)
validate (SndLHS1 (Expr1 x op _ z)) y = hasCorrectCombination1 (cToSet x) op (S.singleton y) (cToSet z)
validate (RHS1 (Expr1 x op y _)) z = hasCorrectCombination1 (cToSet x) op (cToSet y) (S.singleton z)
validate (FstLHS2 (Expr2 _ op y z1 z2)) x = hasCorrectCombination2 (S.singleton x) op (cToSet y) (cToSet z1) (cToSet z2)
validate (SndLHS2 (Expr2 x op _ z1 z2)) y = hasCorrectCombination2 (cToSet x) op (S.singleton y) (cToSet z1) (cToSet z2)
validate (FstRHS2 (Expr2 x op y _ z2)) z1 = hasCorrectCombination2 (cToSet x) op (cToSet y) (S.singleton z1) (cToSet z2)
validate (SndRHS2 (Expr2 x op y z1 _)) z2 = hasCorrectCombination2 (cToSet x) op (cToSet y) (cToSet z1) (S.singleton z2)

-- | Test if there is a combination of digits producing a correct operation.
hasCorrectCombination1 :: S.Set Digit -> Op -> S.Set Digit -> S.Set Digit -> Bool
hasCorrectCombination1 xs op ys zs = foldr go1 False xs

  where

    go1 :: Digit -> Bool -> Bool
    go1 _ True = True
    go1 x False = foldr go2 False ys

      where

        go2 :: Digit -> Bool -> Bool
        go2 _ True = True
        go2 y False = case apply1 x op y of
          Nothing -> False
          Just z -> S.member z zs

-- | Test if there is a combination of digits producing a correct operation.
hasCorrectCombination2 :: S.Set Digit -> Op -> S.Set Digit -> S.Set Digit -> S.Set Digit -> Bool
hasCorrectCombination2 xs op ys zs1 zs2 = foldr go1 False xs

  where

    go1 :: Digit -> Bool -> Bool
    go1 _ True = True
    go1 x False = foldr go2 False ys

      where

        go2 :: Digit -> Bool -> Bool
        go2 _ True = True
        go2 y False = case apply2 x op y of
          Nothing -> False
          Just (z1, z2) -> (S.member z1 zs1) && (S.member z2 zs2)

-- | Simplify an equation.
simplify :: Equation Expr1 Expr2 -> Maybe (Cell Digit)
simplify eq = cFilter (validate eq) (unknown eq)

-- | Increase the available information on the unknown, and record the process.
shrink :: View -> Log (Maybe View)
shrink v = record m v'
  where
    i = unknownIdx (focus v)
    eq = fromIdx (unview v) (focus v)
    mnew = simplify eq
    v' = fmap (\ x -> v { vgrid = (vgrid v) // [(i, x)] }) mnew
    m = Mes { locationId = toLocationId (focus v)
            , ruleId = "simplify"
            , change = case mnew of
                Just new -> describeChange (unknown eq) new
                Nothing -> "failed"
            }

-- | Get all views related to the given view.
others :: View -> [View]
others v = fmap toView eqs
  where
    i = unknownIdx (focus v)
    eqs = filter (elemEq i) allViews
    toView f = v { focus = f }

-- | Test if a view is correct.
check :: View -> Bool
check v = case fromIdx (unview v) (focus v) of
  FstLHS1 (Expr1 (Left x) op (Left y) (Left z)) -> (apply1 x op y) == (Just z)
  SndLHS1 (Expr1 (Left x) op (Left y) (Left z)) -> (apply1 x op y) == (Just z)
  RHS1 (Expr1 (Left x) op (Left y) (Left z)) -> (apply1 x op y) == (Just z)
  FstLHS2 (Expr2 (Left x) op (Left y) (Left z1) (Left z2)) -> (apply2 x op y) == Just (z1, z2)
  SndLHS2 (Expr2 (Left x) op (Left y) (Left z1) (Left z2)) -> (apply2 x op y) == Just (z1, z2)
  FstRHS2 (Expr2 (Left x) op (Left y) (Left z1) (Left z2)) -> (apply2 x op y) == Just (z1, z2)
  SndRHS2 (Expr2 (Left x) op (Left y) (Left z1) (Left z2)) -> (apply2 x op y) == Just (z1, z2)
  _ -> True

-- | Get the grid behind the given view.
unview :: View -> Garam
unview v = G { grid = vgrid v
             , operators = voperators v
             }

-- | Solve a Garam grid, and record how the cells are progessively simplified.
solveGaram :: Integral n
           => n               -- ^ Number of iterations before giving up
           -> Garam           -- ^ Initial grid
           -> Solution Garam
solveGaram limit g0 =
  solveWithIt limit g0 allViews select shrink others check unview

-- | Parse, solve and unparse a Garam puzzle.
processGaram :: Integral n => n -> String -> (String, Maybe (Log String))
processGaram limit input = case fromString input of
  Nothing -> ("unable to parse the input!", Nothing)
  Just g -> case solveGaram limit g of
    Impossible txt -> (T.unpack txt, Nothing)
    Partial g' -> ("unable to solve the puzzle!", Just $ fmap toString g')
    Solved g' -> ("", Just $ fmap toString g')
