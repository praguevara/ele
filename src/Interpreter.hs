{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import Data.Map as M
import Data.Vector as V
import Language

makeLenses ''P

data State = State
  { _m :: Int,
    _variables :: Map Variable Int
  }
  deriving (Show)

makeLenses ''State

initialState :: State
initialState = State {_m = 0, _variables = M.empty}

getVariable :: State -> Variable -> Int
getVariable s v = findWithDefault 0 v (_variables s)

resolveLabel :: P -> State -> Label -> Int
resolveLabel p _ l = (_labels p) M.! l

currentSentence :: P -> State -> Either Int Sentence
currentSentence p s = case _sentences p V.!? (_m s) of
  Nothing -> Left (getVariable s Y)
  Just a -> Right a

incrementPc :: State -> State
incrementPc = over m succ

step :: P -> State -> Either Int State
step p s = case currentSentence p s of
  Left y -> Left y
  Right w -> Right $ case w of
    Inc v -> incrementPc (set variables (insert v (succ (getVariable s v)) (_variables s)) s)
    Dec v -> incrementPc (set variables (insert v (max 0 (pred (getVariable s v))) (_variables s)) s)
    Nop _ -> incrementPc s
    Jnz v l ->
      if (getVariable s v) /= 0
        then (set m (resolveLabel p s l) s)
        else incrementPc s

run :: P -> Int
run p = f (step p initialState)
  where
    f e = case e of
      Left y -> y
      Right s -> f (step p s)
