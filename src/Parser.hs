{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Void
import qualified Language as Lang
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type Line = ((Maybe Lang.Label), Lang.Sentence)

pProgram :: Parser Lang.P
pProgram = do
  lines <- many pLine
  let (ls, ss) = f (zip [0 ..] lines)
  return Lang.P {Lang._labels = M.insert (Lang.Label 'S') (length ss) ls, Lang._sentences = ss}
  where
    f :: [(Int, Line)] -> (M.Map Lang.Label Int, V.Vector Lang.Sentence)
    f ls = foldr (\(i, (l, s)) -> \(m, v) -> (g l i m, V.cons s v)) (M.empty, V.empty) ls

    g :: Maybe Lang.Label -> Int -> M.Map Lang.Label Int -> M.Map Lang.Label Int
    g l i m = case l of
      Nothing -> m
      Just x -> M.insert x i m

pLine :: Parser Line
pLine = do
  maybeLabel <-
    optional
      ( do
          char '('
          l <- pLabel
          string ") "
          return l
      )
  sentence <- pSentence
  return (maybeLabel, sentence)

pLabel :: Parser Lang.Label
pLabel = do
  label <- letterChar
  return (Lang.Label label)

pSentence :: Parser Lang.Sentence
pSentence =
  pJnz <|> do
    v <- pVariable
    ar <- pAr
    return (ar v)

pAr :: Parser (Lang.Variable -> Lang.Sentence)
pAr =
  choice
    [ Lang.Inc <$ string "++",
      Lang.Dec <$ string "--",
      Lang.Nop <$ string "=="
    ]

pJnz :: Parser Lang.Sentence
pJnz = do
  string "IF "
  v <- pVariable
  string " != 0 GOTO "
  l <- pLabel
  return (Lang.Jnz v l)

pVariable :: Parser Lang.Variable
pVariable =
  choice
    [ do
        char 'X'
        i <- read <$> many digitChar :: Parser Int
        return (Lang.X i),
      Lang.Y <$ char 'Y',
      do
        char 'Z'
        i <- read <$> many digitChar :: Parser Int
        return (Lang.Z i)
    ]