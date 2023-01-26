{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I

data Move = R | P | S deriving (Show)

data Outcome = W | L | D deriving (Show)

-- p1
-- main :: IO ()
-- main = do
--  input <- I.readFile "in"
--  let split = T.lines input
--  let moves = fmap (\x -> cnv $ T.splitOn " " x) split
--  let games = fmap (\(x, y) -> (decodeOponent x, decodeOwn y)) moves
--  let outcomes = fmap (\(x, y) -> evalOutcome x y) games
--  let res = L.sum outcomes
--  print res

cnv :: [T.Text] -> (T.Text, T.Text)
cnv [x, y] = (x, y)
cnv _ = error "incorrect split"

decodeOponent :: T.Text -> Move
decodeOponent "A" = R
decodeOponent "B" = P
decodeOponent "C" = S
decodeOponent _ = error "impossible"

decodeOwn :: T.Text -> Move
decodeOwn "Y" = P
decodeOwn "X" = R
decodeOwn "Z" = S
decodeOwn _ = error "impossible"

scoreMove :: Move -> Int
scoreMove x = case x of
  R -> 1
  P -> 2
  S -> 3

scoreOutcome :: Outcome -> Int
scoreOutcome x = case x of
  W -> 6
  L -> 0
  D -> 3

evalOutcome :: Move -> Move -> Int
evalOutcome R R = scoreMove R + scoreOutcome D
evalOutcome R P = scoreMove P + scoreOutcome W
evalOutcome R S = scoreMove S + scoreOutcome L
evalOutcome P R = scoreMove R + scoreOutcome L
evalOutcome P P = scoreMove P + scoreOutcome D
evalOutcome P S = scoreMove S + scoreOutcome W
evalOutcome S R = scoreMove R + scoreOutcome W
evalOutcome S P = scoreMove P + scoreOutcome L
evalOutcome S S = scoreMove S + scoreOutcome D

-- p2
main :: IO ()
main = do
  input <- I.readFile "in"
  let split = T.lines input
  let moves = fmap (\x -> cnv $ T.splitOn " " x) split
  let games = fmap decodeGame moves
  let results = fmap (\(x, y) -> evalOutcome x y) games
  let res = L.sum results
  print res

decodeGame :: (T.Text, T.Text) -> (Move, Move)
decodeGame (x, y) =
  let op = decodeOponent x
      out = decodeOutcome y
      own = correctOutcome out op
   in (op, own)

decodeOutcome :: T.Text -> Outcome
decodeOutcome "X" = L
decodeOutcome "Y" = D
decodeOutcome "Z" = W
decodeOutcome _ = error "impossible"

correctOutcome :: Outcome -> Move -> Move
correctOutcome L P = R
correctOutcome L R = S
correctOutcome L S = P
correctOutcome D x = x
correctOutcome W P = S
correctOutcome W S = R
correctOutcome W R = P
