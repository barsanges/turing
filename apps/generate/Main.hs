{- |
   Module      : Main
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Parses command line options to generate new puzzles.
-}

module Main where

import Options.Applicative
import Data.Text.IO as T
import Games.Garam.Generate ( generateGaram )

data Args = Args
  { seed :: Int
  , npuzzles :: Int
  , niter :: Int
  , nvalues :: Int
  , limit :: Int
  , outputFile :: String
  }

argsParser :: Parser Args
argsParser = Args
  <$> (option auto
       ( short 's'
         <> metavar "SEED"
         <> help "random generator seed"
       ))
  <*> (option auto
       ( short 'p'
         <> metavar "NPUZZLES"
         <> help "number of puzzles to generate"
       ))
  <*> (option auto
       ( short 'n'
         <> metavar "NITER"
         <> help "number of iterations allowed to solve one puzzle"
       ))
  <*> (option auto
       ( short 'v'
         <> metavar "NVALUES"
         <> help "number of values known in each puzzle"
       ))
  <*> (option auto
       ( short 'l'
         <> metavar "LIMIT"
         <> help "number of attempts allowed before giving up"
       ))
  <*> (strOption
       ( short 'o'
         <> metavar "FOUT"
         <> help "name of the output file"
       ))

-- | Command line parser for 'turing'.
args :: ParserInfo Args
args = info (argsParser <**> helper)
  ( fullDesc
  <> header "turing - generate"
  <> progDesc "Generate mathematical puzzles" )

-- | Application entry point.
main :: IO ()
main = do
  cli <- execParser args
  let res = generateGaram (seed cli) (npuzzles cli) (niter cli) (nvalues cli) (limit cli)
  T.writeFile (outputFile cli) res