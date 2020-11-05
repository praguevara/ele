module Language where

import qualified Data.Map as M
import qualified Data.Vector as V

data Label = Label Char Int
  deriving (Eq, Ord)

instance Show Label where
  show (Label c i) = concat ["(", [c], show i, ")"]

data Variable = X Int | Y | Z Int
  deriving (Eq, Ord)

instance Show Variable where
  show (X i) = concat ["X", show i]
  show Y = "Y"
  show (Z i) = concat ["Z", show i]

data Sentence = Inc Variable | Dec Variable | Nop Variable | Jnz Variable Label

instance Show Sentence where
  show (Inc v) = concat [show v, "++"]
  show (Dec v) = concat [show v, "--"]
  show (Nop v) = concat [show v, "=="]
  show (Jnz v l) = concat ["IF ", show v, " != 0 GOTO ", show l]

data P = P
  { _labels :: M.Map Label Int,
    _sentences :: V.Vector Sentence
  }

instance Show P where
  show P {_labels = ls, _sentences = ss} = unlines ["\tLabels:", showLabels ls, "\tSentences:", showSentences ss]
    where
      showLabels ls = unlines $ (\(l, i) -> concat [show i, ":\t", show l]) <$> (M.toList ls)
      showSentences ss = unlines ((\(i, s) -> concat [show i, ":\t", show s]) <$> zip [0 ..] (V.toList ss))