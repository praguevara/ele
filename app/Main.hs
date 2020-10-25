{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Data.Text (pack)
import Interpreter
import Language
import Options.Applicative
import Parser (pProgram)
import System.Environment
import Text.Megaparsec (parse)

data Options = Options
  { file :: FilePath,
    input :: [Int]
  }

opts :: Parser Options
opts =
  Options
    <$> strOption
      ( short 'F'
          <> long "file"
          <> metavar "FILE"
          <> help "Source file"
      )
    <*> option
      auto
      ( long "input"
          <> value [0]
          <> metavar "X"
          <> help "Initial value of the X variables"
      )

main :: IO ()
main = do
  os <- execParser (info (helper <*> opts) (fullDesc <> progDesc "An L interpreter. By praguevara."))
  contents <- pack <$> readFile (file os)
  let p = parse pProgram "L" contents
  print p
  print (run (input os) <$> p)
