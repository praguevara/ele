{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)
import Interpreter
import Options.Applicative
import Parser (pProgram)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error

data Options = Options
  { file :: FilePath,
    input :: [Int]
  }

opts :: Parser Options
opts =
  Options
    <$> argument str
      ( metavar "FILE"
          <> help "Source file"
      )
    <*> option
      auto
      ( long "input"
          <> short 'X'
          <> metavar "INPUT"
          <> value [0]
          <> help "[3, 1] means that X = 3, X1 = 1"
      )

main :: IO ()
main = do
  os <- execParser (info (helper <*> opts) (fullDesc <> progDesc "An L interpreter. By praguevara."))
  contents <- pack <$> readFile (file os)
  case parse pProgram "L" contents of
    Left err -> putStrLn (errorBundlePretty err)
    Right p -> do
      let (y, trace) = run (input os) p
      mapM_ print trace
      putStrLn ("Y = " ++ show y)
