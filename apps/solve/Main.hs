{- |
   Module      : Main
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Parses command line options and calls the appropriate solver.
-}

module Main where

import Options.Applicative
import Data.Text as T
import Commons.Log ( dropLog, getLog )
import Games.Garam.Solve ( processGaram )
import Games.Futoshiki ( processFutoshiki )
import Games.Sudoku ( processSudoku )

data Puzzle = Garam
            | Futoshiki
            | Sudoku

data Input =  InputFile String
            | StdIn

data Output = OutputFile String
            | StdOut

data Args = Args
  { puzzle :: Puzzle
  , input :: Input
  , output :: Output
  , n :: Int
  , showLog :: Bool
  }

argsParser :: Parser Args
argsParser = Args
  <$> ((flag' Garam
        ( long "garam"
          <> help "Solve a Garam puzzle"))
       <|> (flag' Futoshiki
            ( long "futoshiki"
              <> help "Solve a Futoshiki puzzle"))
       <|> (flag' Sudoku
            ( long "sudoku"
              <> help "Solve a Sudoku puzzle"))
      )
  <*> ( (InputFile <$> strOption
         ( long "fin"
           <> short 'i'
           <> metavar "FIN"
           <> help "Solve the puzzle in the file 'FIN'"
         ))
        <|> pure StdIn
      )
  <*> ( (OutputFile <$> strOption
         ( long "fout"
           <> short 'o'
           <> metavar "FOUT"
           <> help "Write the result to the file 'FOUT' instead of sending it to stdout"
         ))
        <|> pure StdOut
      )
  <*> (option auto
       ( short 'n'
         <> metavar "N"
         <> help "number of iterations allowed to solve the puzzle"
       ))
  <*> (switch
       (long "log"
       <> short 'l'
       <> help "print the log on stdout"
       ))

-- | Command line parser for 'turing'.
args :: ParserInfo Args
args = info (argsParser <**> helper)
  ( fullDesc
  <> header "turing - solve"
  <> progDesc "Solve a mathematical puzzle" )

-- | Application entry point.
main :: IO ()
main = do
  cli <- execParser args
  s <- case input cli of
         InputFile f -> readFile f
         StdIn -> getContents
  let (msg, mres) = case puzzle cli of
        Garam -> processGaram (n cli) s
        Futoshiki -> processFutoshiki (n cli) s
        Sudoku -> processSudoku (n cli) s
  if msg /= ""
    then putStrLn msg
    else pure ()
  case mres of
    Nothing -> pure ()
    Just res -> outLog *> outResult

      where

        outLog = case showLog cli of
          True -> putStrLn (T.unpack $ getLog res)
          False -> pure ()

        outResult = case output cli of
          OutputFile f -> writeFile f (dropLog res)
          StdOut -> putStrLn (dropLog res)
