{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace
import Language

makeLenses ''P

data State = State
  { _m :: Int,
    _variables :: M.Map Variable Int
  }
  deriving (Show)

makeLenses ''State

initialState :: [Int] -> State
initialState vs = State {_m = 0, _variables = M.fromList ((\(i, x) -> (X i, x)) <$> (zip [0 ..] vs))}

getVariable :: State -> Variable -> Int
getVariable s v = M.findWithDefault 0 v (_variables s)

resolveLabel :: P -> State -> Label -> Int
resolveLabel p _ l = (_labels p) M.! l

currentSentence :: P -> State -> Either Int Sentence
currentSentence p s = case (traceShowId $ _sentences p V.!? (_m s)) of
  Nothing -> Left (getVariable s Y)
  Just a -> Right a

incrementPc :: State -> State
incrementPc = over m succ

step :: P -> State -> Either Int State
step p s = case currentSentence p (traceShowId s) of
  Left y -> Left y
  Right w -> Right $ case w of
    Inc v -> incrementPc (set variables (M.insert v (succ (getVariable s v)) (_variables s)) s)
    Dec v -> incrementPc (set variables (M.insert v (max 0 (pred (getVariable s v))) (_variables s)) s)
    Nop _ -> incrementPc s
    Jnz v l ->
      if (getVariable s v) /= 0
        then (set m (resolveLabel p s l) s)
        else incrementPc s

run :: [Int] -> P -> Int
run vs p = f (step p (initialState vs))
  where
    f e = case e of
      Left y -> y
      Right s -> f (step p s)
