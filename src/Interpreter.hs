{-# LANGUAGE TemplateHaskell #-}

module Interpreter where

import Control.Lens
import Control.Monad.Trans.Writer.Strict
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
      [ concat ["m = ", show m],
        concat ["variables: ", (show . M.toList) vs]
      ]

initialState :: [Int] -> State
initialState vs =
  State
    { _m = 0,
      _variables = M.fromList ((\(i, x) -> (X i, x)) <$> (zip [0 ..] vs))
    }

getVariable :: State -> Variable -> Int
getVariable s v = M.findWithDefault 0 v (_variables s)

resolveLabel :: P -> State -> Label -> Int
resolveLabel p _ l = (_labels p) M.! l

currentSentence :: P -> State -> Either Int Sentence
currentSentence p s = case _sentences p V.!? (_m s) of
  Nothing -> Left (getVariable s Y)
  Just a -> Right a

incrementPc :: State -> State
incrementPc = over m succ

step :: P -> State -> Writer [State] (Either Int State)
step p s = case currentSentence p s of
  Left y -> return $ Left y
  Right w -> do
    tell [s]
    return $
      Right $ case w of
        Inc v -> incrementPc (set variables (M.insert v (succ (getVariable s v)) (_variables s)) s)
        Dec v -> incrementPc (set variables (M.insert v (max 0 (pred (getVariable s v))) (_variables s)) s)
        Nop _ -> incrementPc s
        Jnz v l ->
          if (getVariable s v) /= 0
            then set m (resolveLabel p s l) s
            else incrementPc s

run :: [Int] -> P -> (Int, [State])
run vs p = (runWriter . f) (step p (initialState vs))
  where
    f :: Writer [State] (Either Int State) -> Writer [State] Int
    f x = do
      e <- x
      case e of
        Left y -> return y
        Right s -> f (step p s)