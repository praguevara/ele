module Language where

import qualified Data.Map as M
import qualified Data.Vector as V

newtype Label = Label Char
  deriving (Eq, Ord)

instance Show Label where
  show (Label c) = concat ["(", [c], ")"]

data Variable = X Int | Y | Z Int
  deriving (Eq, Ord)

instance Show Variable where
  show v = case v of
    X i -> concat ["X", hideZero i]
    Y -> "Y"
    Z i -> concat ["Z", hideZero i]
    where
      hideZero i = if i == 0 then "" else show i

data Sentence = Inc Variable | Dec Variable | Nop Variable | Jnz Variable Label

instance Show Sentence where
  show s = case s of
    Inc v -> concat [show v, "++"]
    Dec v -> concat [show v, "--"]
    Nop v -> concat [show v, "=="]
    Jnz v l -> concat ["IF ", show v, " != 0 GOTO ", show l]

data P = P
  { _labels :: M.Map Label Int,
    _sentences :: V.Vector Sentence
  }

instance Show P where
  show P {_labels = ls, _sentences = ss} = unlines ["\tLabels:", showLabels ls, "\tSentences:", showSentences ss]
    where
      showLabels ls = unlines $ (\(l, i) -> concat [show i, ":\t", show l]) <$> (M.toList ls)
      showSentences ss = unlines ((\(i, s) -> concat [show i, ":\t", show s]) <$> zip [0 ..] (V.toList ss))