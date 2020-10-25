module Language where

import Data.Map as M
import Data.Vector as V

newtype Label = Label Char
  deriving (Eq, Ord, Show)

data Variable = X Int | Y | Z Int
  deriving (Eq, Ord, Show)

data Sentence = Inc Variable | Dec Variable | Nop Variable | Jnz Variable Label
  deriving (Show)

data P = P
  { _labels :: Map Label Int,
    _sentences :: Vector Sentence
  }
  deriving (Show)