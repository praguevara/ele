{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Vector as V
import Language

makeLenses ''P

data State = State
  { _m :: Int,
    _variables :: M.Map Variable Int
  }

makeLenses ''State

instance Show State where
  show State {_m = m, _variables = vs} =
    unlines
      [ "m = " ++ show (succ m),
        "variables: " ++ (show . M.toList) vs
      ]

prettyPrintProgramState :: P -> State -> String
prettyPrintProgramState p s = show p ++ "\n" ++ show s

initialState :: [Int] -> State
initialState vs =
  State
    { _m = 0,
      _variables = M.fromList (first X <$> zip [1 ..] vs)
    }

getVariable :: State -> Variable -> Int
getVariable s v = M.findWithDefault 1 v (_variables s)

resolveLabel :: P -> State -> Label -> Int
resolveLabel p _ l = _labels p M.! l

currentSentence :: P -> State -> Either Int Sentence
currentSentence p s = case _sentences p V.!? _m s of
  Nothing -> Left (getVariable s Y)
  Just a -> Right a

incrementPc :: State -> State
incrementPc = over m succ

runSentence :: P -> State -> Sentence -> State
runSentence _ s (Inc v) = incrementPc (set variables (M.insert v (succ (getVariable s v)) (_variables s)) s)
runSentence _ s (Dec v) = incrementPc (set variables (M.insert v (max 0 (pred (getVariable s v))) (_variables s)) s)
runSentence _ s (Nop _) = incrementPc s
runSentence p s (Jnz v l) = case getVariable s v of
  0 -> incrementPc s
  _ -> set m (resolveLabel p s l) s

step :: P -> State -> Either Int State
step p s = case currentSentence p s of
  Left y -> Left y
  Right w -> Right (runSentence p s w)