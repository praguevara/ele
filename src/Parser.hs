{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import qualified Language as Lang
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

sc :: Parser ()
sc =
  L.space
    hspace1
    (L.skipLineComment "#" >> some newline >> pure ())
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol' sc

type Line = (Maybe Lang.Label, Lang.Sentence)

pProgram :: Parser Lang.P
pProgram = do
  lines' <- many pLine
  eof
  let (ls, ss) = f (zip [0 ..] lines')
  pure Lang.P {Lang._labels = M.insert (Lang.Label 'S' 1) (V.length ss) ls, Lang._sentences = ss}
  where
    f ls = foldr (\(i, (l, s)) (m, v) -> (g l i m, V.cons s v)) (M.empty, V.empty) ls

    g l i m = case l of
      Nothing -> m
      Just x -> M.insert x i m

pLine :: Parser Line
pLine = do
  lexeme
    ( do
        maybeLabel <- (lexeme . optional) (between (symbol "(") (symbol ")") pLabel)
        sentence <- pSentence
        _ <- some newline
        pure (maybeLabel, sentence)
    )

pLabel :: Parser Lang.Label
pLabel = do
  (a, b) <- pXi (choice (char' <$> ['A', 'B', 'C', 'D', 'S']))
  pure (Lang.Label a b)

pSentence :: Parser Lang.Sentence
pSentence =
  pJnz <|> do
    v <- pVariable
    ar <- pAr
    pure (ar v)

pAr :: Parser (Lang.Variable -> Lang.Sentence)
pAr =
  choice
    [ Lang.Inc <$ symbol "++",
      Lang.Dec <$ symbol "--",
      Lang.Nop <$ symbol "=="
    ]

pJnz :: Parser Lang.Sentence
pJnz = do
  _ <- symbol "IF"
  v <- pVariable
  traverse_ symbol ["!=", "0", "GOTO"]
  Lang.Jnz v <$> pLabel

pVariable :: Parser Lang.Variable
pVariable =
  (lexeme . choice)
    [ Lang.X <$> (snd <$> pXi (symbol "X")),
      Lang.Y <$ symbol "Y",
      Lang.Z <$> (snd <$> pXi (symbol "Z"))
    ]

pXi :: Num b => Parser a -> Parser (a, b)
pXi x = do
  l <- x
  i <- fromInteger <$> L.decimal <|> pure 1
  pure (l, i)
