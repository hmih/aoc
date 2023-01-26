{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as I

data Row = Row Range Range deriving (Show)

data Range = Range Int Int deriving (Show)

-- 1000 too high
main :: IO ()
main = do
  contents <- I.readFile "in"
  let split = T.lines contents
      pairs = fmap pairUp split
      overlaps = L.sum $ fmap findOverlap pairs
  print overlaps

pairUp :: T.Text -> Row
pairUp x =
  let (l : r : _) = T.split (== ',') x
      (l1 : l2 : _) = fmap (\x -> read (T.unpack x) :: Int) $ T.split (== '-') l
      (r1 : r2 : _) = fmap (\x -> read (T.unpack x) :: Int) $ T.split (== '-') r
   in Row (Range l1 l2) (Range r1 r2)

-- p1
-- findOverlap :: Row -> Int
-- findOverlap (Row (Range l1 u1) (Range l2 u2)) = if l1 <= l2 && u1 >= u2 then 1 else if l2 <= l1 && u2 >= u1 then 1 else 0

-- p2
findOverlap :: Row -> Int
findOverlap (Row (Range l1 u1) (Range l2 u2)) =
  let c1 = l1 <= l2 && u1 >= l2
      c2 = l1 <= u2 && u1 >= u2

      c3 = l2 <= l1 && u2 >= l1
      c4 = l2 <= u1 && u2 >= u1

      c5 = l1 <= l2 && u1 >= u2
      c6 = l2 <= l1 && u2 >= u1

   in if c1 || c2 || c3 || c4 || c5 || c6 then 1 else 0
