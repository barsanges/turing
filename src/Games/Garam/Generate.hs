{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Games.Garam.Generate
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Generate a Garam puzzle.
-}

module Games.Garam.Generate (
  generateGaram
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Random as R
import Commons.Digit ( Digit(..) )
import Commons.Log ( dropLog )
import Games.Garam.Solve ( Op(..), Garam, fromElements, getValues, setValue,
                           solveGaram )

-- | A Garam grid that may be solved.
data Result = Res (V.Vector Op) (IM.IntMap Digit)
  deriving Show

-- | Generate a random value from the range 'rng', using the conversion function
-- 'f'.
rand :: R.RandomGen g => (Int -> a) -> (Int, Int) -> g -> (a, g)
rand f rng gen = (f x, gen')
  where
    (x, gen') = R.uniformR rng gen

-- | Turn an int to an operator.
opFromInt :: Int -> Op
opFromInt 0 = Plus
opFromInt 1 = Mul
opFromInt _ = Minus

-- | Randomly pick a value in a vector.
pick :: R.RandomGen g => V.Vector a -> g -> (a, g)
pick xs gen = (xs V.! idx, gen')
  where
    (idx, gen') = R.uniformR (0, (V.length xs) - 1) gen

-- | Generate a vector of random operator for a Garam grid.
generateOps :: R.RandomGen g => g -> (V.Vector Op, g)
generateOps initGen = foldr go (initVec, initGen) [0..19]
  where
    initVec = V.replicate 20 Plus

    go :: R.RandomGen g => Int -> (V.Vector Op, g) -> (V.Vector Op, g)
    go n (xs, gen) = (xs V.// [(n, op)], gen')
      where
        (op, gen') = if S.member n (S.fromList [2, 3, 4, 5, 13, 14, 15, 16])
                     then rand opFromInt (0, 1) gen -- +, *
                     else rand opFromInt (0, 2) gen -- +, *, -

-- | Recursively populate a Garam grid with values compatible with
-- the ones already in place.
generateValues :: R.RandomGen g
               => V.Vector Op
               -> Int
               -> Int
               -> g
               -> (Maybe (IM.IntMap Digit), g)
generateValues ops nshrink nvalues initGen =
  case fromElements ops IM.empty of
    Nothing -> (Nothing, initGen) -- Should not happen in practice
    Just initGrid -> go IM.empty initGrid nvalues initGen

  where

    go vals g n gen
      | n <= 0 = (Just vals, gen)
      | otherwise = go vals' g'' (n-1) gen2
      where
        g' = simplify g
        freeIndexes = IS.difference allIndexes (IM.keysSet vals)
        (idx, gen1) = pick (V.fromList $ IS.toList freeIndexes) gen
        xs = getValues g
        (x, gen2) = pick (xs V.! idx) gen1
        vals' = IM.insert idx x vals
        g'' = setValue idx x g'

    allIndexes = IS.fromDistinctAscList [0..43]

    simplify :: Garam -> Garam
    simplify grid = case solveGaram nshrink grid of
                      Left grid' -> dropLog grid'
                      Right grid' -> dropLog grid'

-- | Test if a grid may be solved in 'niter' iterations.
validate :: Integral n => n -> Garam -> Bool
validate niter garam = case solveGaram niter garam of
                           Left _ -> False
                           Right _ -> True

-- | Try to generate a solvable Garam grid with 'nvalues' known at the
-- beginning. Make only one attempt.
generateOne :: (Integral n, R.RandomGen g)
            => n
            -> Int
            -> Int
            -> g
            -> (Maybe Result, g)
generateOne niter nshrink nvalues gen = (res, gen2)
  where
    (ops, gen1) = generateOps gen
    (mvals, gen2) = generateValues ops nshrink nvalues gen1
    -- Ugly
    res = case mvals of
      Just vals -> case fmap (validate niter) (fromElements ops vals) of
                     Just True -> Just (Res ops vals)
                     _ -> Nothing
      Nothing -> Nothing

-- | Try to generate a solvable Garam grid with 'nvalues' known at the
-- beginning. Give up if it takes more than 'limit' attempts to find a
-- grid.
generateLimit :: (Integral n, R.RandomGen g)
              => n
              -> Int
              -> Int
              -> Int
              -> g
              -> (Maybe Result, g)
generateLimit niter nshrink nvalues limit initGen = go 0 initGen
  where
    go :: R.RandomGen g => Int -> g -> (Maybe Result, g)
    go i gen = if i >= limit
               then (Nothing, gen)
               else case res of
                      Just _ -> (res, gen')
                      Nothing -> go (i+1) gen'
      where
        (res, gen') = generateOne niter nshrink nvalues gen

-- | Return a text corresponding to a cell drawn with Latex/TikZ.
latexCell :: T.Text -> Maybe T.Text -> T.Text -> T.Text
latexCell n mplace value =
  T.replace "VALUE" value (T.replace "NODE" n placed)
  where
    base = "\\node[draw,minimum width=1cm,minimum height=1cm,PLACE] (NODE) {VALUE};\n"
    placed = case mplace of
      Just place -> T.replace "PLACE" place base
      Nothing -> "\\node[draw,minimum width=1cm,minimum height=1cm] (NODE) at (0, 0) {VALUE};\n"

-- | Return a text corresponding to an operator drawn with Latex/TikZ.
latexOp :: (T.Text, T.Text) -> Op -> T.Text
latexOp (n1, n2) op =
  T.replace "#2" n2 (T.replace "#1" n1 base)
  where
    base = case op of
      Plus -> plus
      Mul -> mul
      Minus -> minus
    plus = "\\draw[line width=2pt] ($(#1)!0.5!(#2) + (-0.15,0.0)$) -- ($(#1)!0.5!(#2) + (0.15,0.0)$);\n\
           \\\draw[line width=2pt] ($(#1)!0.5!(#2) + (0.0,-0.15)$) -- ($(#1)!0.5!(#2) + (0.0,0.15)$);\n"
    mul ="\\draw[line width=2pt] ($(#1)!0.5!(#2) + (-0.1,0.1)$) -- ($(#1)!0.5!(#2) + (0.1,-0.1)$);\n\
          \\\draw[line width=2pt] ($(#1)!0.5!(#2) + (-0.1,-0.1)$) -- ($(#1)!0.5!(#2) + (0.1,0.1)$);\n"
    minus = "\\draw[line width=2pt] ($(#1)!0.5!(#2) + (-0.15,0.0)$) -- ($(#1)!0.5!(#2) + (0.15,0.0)$);\n"

-- | Return a text corresponding to an equal sign drawn with Latex/TikZ.
latexEq :: (T.Text, T.Text) -> T.Text
latexEq (n1, n2) =
  T.replace "#2" n2 (T.replace "#1" n1 base)
  where
    base = "\\draw[line width=2pt] ($(#1)!0.5!(#2) + (-0.15,0.07)$) -- ($(#1)!0.5!(#2) + (0.15,0.07)$);\n\
           \\\draw[line width=2pt] ($(#1)!0.5!(#2) + (-0.15,-0.07)$) -- ($(#1)!0.5!(#2) + (0.15,-0.07)$);\n"

-- | Turn a Garam grid into a Latex string.
toLatex :: Result -> T.Text
toLatex (Res ops values) = T.concat [open, values', ops', eqSigns, close]
  where
    open = "\\begin{center}\\begin{tikzpicture}\n"
    close = "\\end{tikzpicture}\\end{center}"
    eqSigns = T.concat $ fmap latexEq [ ("1", "2")
                                      , ("4", "5")
                                      , ("8", "9")
                                      , ("6", "11")
                                      , ("7", "12")
                                      , ("9", "13")
                                      , ("10", "14")
                                      , ("16", "17")
                                      , ("19", "20")
                                      , ("21", "24")
                                      , ("22", "27")
                                      , ("24", "25")
                                      , ("27", "28")
                                      , ("31", "32")
                                      , ("29", "34")
                                      , ("30", "35")
                                      , ("32", "36")
                                      , ("33", "37")
                                      , ("39", "40")
                                      , ("42", "43") ]

    -- The order matters for TikZ to be able to place the nodes with respect
    -- to each other!
    cells = [ (0, Nothing)
            , (1, Just "right of=0")
            , (2, Just "right of=1")
            , (6, Just "below of=0")
            , (7, Just "below of=2")
            , (8, Just "right of=7")
            , (9, Just "right of=8")
            , (3, Just "above of=9")
            , (4, Just "right of=3")
            , (5, Just "right of=4")
            , (10, Just "below of=5")
            , (11, Just "below of=6")
            , (12, Just "below of=7")
            , (13, Just "below of=9")
            , (14, Just "below of=10")
            , (15, Just "below of=11")
            , (16, Just "right of=15")
            , (17, Just "right of=16")
            , (18, Just "below of=13")
            , (19, Just "right of=18")
            , (20, Just "right of=19")
            , (21, Just "below of=16")
            , (22, Just "below of=19")
            , (24, Just "below of=21")
            , (23, Just "left of=24")
            , (25, Just "right of=24")
            , (27, Just "below of=22")
            , (26, Just "left of=27")
            , (28, Just "right of=27")
            , (29, Just "below of=23")
            , (30, Just "below of=25")
            , (31, Just "right of=30")
            , (32, Just "right of=31")
            , (33, Just "below of=28")
            , (34, Just "below of=29")
            , (35, Just "below of=30")
            , (36, Just "below of=32")
            , (37, Just "below of=33")
            , (38, Just "below of=34")
            , (39, Just "right of=38")
            , (40, Just "right of=39")
            , (41, Just "below of=36")
            , (42, Just "right of=41")
            , (43, Just "right of=42") ]
    values' = T.concat $ fmap goValues cells

    goValues :: (Int, Maybe T.Text) -> T.Text
    goValues (n, mtxt) = case IM.lookup n values of
      Just x -> latexCell (T.pack $ show n) mtxt (T.pack $ show x)
      Nothing -> latexCell (T.pack $ show n) mtxt (" ")

    ops' = T.concat $ fmap goOps (zip [0..19] opsPositions)
    opsPositions = [ ("0", "1")
                   , ("3", "4")
                   , ("0", "6")
                   , ("2", "7")
                   , ("3", "9")
                   , ("5", "10")
                   , ("7", "8")
                   , ("15", "16")
                   , ("18", "19")
                   , ("16", "21")
                   , ("19", "22")
                   , ("23", "24")
                   , ("26", "27")
                   , ("23", "29")
                   , ("25", "30")
                   , ("26", "32")
                   , ("28", "33")
                   , ("30", "31")
                   , ("38", "39")
                   , ("41", "42") ]

    goOps :: (Int, (T.Text, T.Text)) -> T.Text
    goOps (n, txt) = latexOp txt (ops V.! n)

-- | Try to generate 'ngrids' Garam grids.
generateGaram :: (Integral n, Show n)
              => Int
              -> Int
              -> n
              -> Int
              -> Int
              -> Int
              -> T.Text
generateGaram seed ngrids niter nshrink nvalues limit = T.append header grids
  where
    header = T.pack ("Graine : " ++ show seed
                     ++ "\nNombre d'itérations : " ++ show niter
                     ++ "\nNombre de simplifications : " ++ show nshrink
                     ++ "\nNombre de valeurs : " ++ show nvalues
                     ++ "\nLimite : " ++ show limit
                     ++ "\n")
    initGen = R.mkStdGen seed
    grids = T.intercalate "\n" (fst $ foldr go ([], initGen) [1..ngrids])

    go :: R.RandomGen g => Int -> ([T.Text], g) -> ([T.Text], g)
    go _ (xs, gen) = case generateLimit niter nshrink nvalues limit gen of
      (Just g, gen') -> ((toLatex g):xs, gen')
      (Nothing, gen') -> (xs, gen')
