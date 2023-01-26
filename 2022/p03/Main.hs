{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I

data Sack = Sack String String deriving (Show)

-- p1
-- main :: IO ()
-- main = do
--  input <- I.readFile "in"
--  let split = fmap T.unpack $ T.lines input
--      sacks = fmap mkSack split
--      vals = fmap findDuplicate sacks
--      prios = L.sum $ fmap priority $ L.concat vals
--  print prios

--  p2
main :: IO ()
main = do
  input <- I.readFile "in"
  let split = fmap T.unpack $ T.lines input
      groups = combine split
      vals = fmap findDuplicate groups
      prios = L.sum $ fmap priority $ L.concat vals
  print prios

-- p2
combine :: [String] -> [(String, String, String)]
combine [] = []
combine [x] = [(x, "", "")]
combine [x, y] = [(x, y, "")]
combine [x, y, z] = [(x, y, z)]
combine (x:y:z:rest) = [(x, y, z)] ++ combine rest

findDuplicate :: (String, String, String) -> String
findDuplicate (x, y, z) = L.nub $ L.intersect z $ L.intersect x y

-- p1
-- findDuplicate :: (String, String) -> String
-- findDuplicate (xs, ys) = L.nub $ L.intersect xs ys

priority :: Char -> Int
priority x =
  let num = C.ord x
   in if C.isLower x then num - 96 else num - 38

mkSack :: String -> (String, String)
mkSack x =
  let len = L.length x
      isEven = (len `mod` 2) == 0
      middle = (len `div` 2)
      (l, r) = L.splitAt middle x
   in if isEven then (l, r) else (l, tail r)
