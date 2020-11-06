{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)
import Interpreter
import qualified Language as Lang
import Options.Applicative
import Parser (pProgram)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

data Options = Options
  { _mode :: Mode,
    _file :: FilePath
  }

data Mode = Run [Int] | Encode

runOpt :: Parser Mode
runOpt =
  Run
    <$> option
      auto
      ( long "input"
          <> short 'X'
          <> metavar "INPUT"
          <> value [0]
          <> help "[3, 1] means that X(1) = 3, X2 = 1"
      )

encodeOpt :: Parser Mode
encodeOpt =
  flag'
    Encode
    ( long "encode"
        <> short 'C'
        <> help "Encode the program"
    )

opts :: Parser Options
opts =
  Options
    <$> (runOpt <|> encodeOpt)
    <*> argument
      str
      ( metavar "FILE"
          <> help "Source file"
      )

main :: IO ()
main = do
  os <- execParser (info (helper <*> opts) (fullDesc <> progDesc "An L interpreter. By praguevara."))
  contents <- pack <$> readFile (_file os)
  case parse pProgram "L" contents of
    Left err -> putStrLn (errorBundlePretty err)
    Right p -> case _mode os of
      (Run xs) -> userRun p xs
      _ -> error "Unimplemented"

userRun :: Lang.P -> [Int] -> IO ()
userRun p xs = do
  let is = initialState xs
  r (Right is)
  where
    r :: Either Int State -> IO ()
    r x = do
      case x of
        Left i -> putStrLn ("Y = " ++ show i)
        Right s -> do
          putStrLn (prettyPrintProgramState p s)
          putStrLn "Press <Enter> to continue..."
          _ <- getLine
          r (step p s)
