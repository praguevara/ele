{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Data.Text (pack)
import Interpreter
import Language
import Options.Applicative
import Parser (pProgram)
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
          <> short 'X'
          <> value [0]
          <> help "[3, 1] means that X = 3, X1 = 1"
      )

main :: IO ()
main = do
  os <- execParser (info (helper <*> opts) (fullDesc <> progDesc "An L interpreter. By praguevara."))
  contents <- pack <$> readFile (file os)
  let p = parse pProgram "L" contents
  case p of
    Left _ -> return ()
    Right p -> do
      print p
      let (y, trace) = run (input os) p
      mapM_ print trace
      putStrLn ("Y = " ++ show y)
