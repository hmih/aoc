{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace
import GHC.Base (divInt)
import Data.Bits (shiftL)
import qualified Data.Text as T
import qualified Data.Text.IO as I

type BinaryNumber = T.Text
type BinaryList = [Int]

data Criteria = One | Zero | Equal 
  deriving (Show)

-- too low
-- 3063114
-- 4827636

main :: IO ()
main = do
  raw <- I.readFile "in"
  let lines = T.lines raw
      lineLength = T.length $ lines !! 0
      fileSize = length lines
      halfSize = divInt fileSize 2

  if (all (\l -> T.length l == lineLength) lines) then print "ok" else error "bad"

  let cols = [0..(lineLength - 1)]
      unpacked = unpackBN lines

  let oxygen = binaryToDecimal $ mcb unpacked 0
  let co2 = binaryToDecimal $ lcb unpacked 0
  let lifeSupport = oxygen * co2

  print $ "oxygen: " ++ show oxygen
  print $ "co2: " ++ show co2
  print $ "lifeSupport: " ++ show lifeSupport

mcb :: [BinaryList] -> Int -> BinaryList
mcb [x] _ = x
mcb xs col =
  let common = mostCommonInCol xs col
      filtered = case common of
        One -> filter (\b -> (b !! col) == 1) xs
        Zero -> filter (\b -> (b !! col) == 0) xs
        Equal -> filter (\b -> (b !! col) == 1) xs
  in mcb filtered (col + 1)

lcb :: [BinaryList] -> Int -> BinaryList
lcb [x] _ = x
lcb xs col =
  let common = mostCommonInCol xs col
      filtered = case common of
        One -> filter (\b -> (b !! col) == 0) xs
        Zero -> filter (\b -> (b !! col) == 1) xs
        Equal -> filter (\b -> (b !! col) == 0) xs
  in lcb filtered (col + 1)

binaryToDecimal :: BinaryList -> Int
binaryToDecimal b = sum $ map (\(f, v) -> f * v) $ zip (reverse b) (map (2^) [0..])

unpackBN :: [BinaryNumber] -> [BinaryList]
unpackBN xs = map (\x -> map (\x -> read [x] :: Int) $ T.unpack x) xs

mostCommonInCol :: [BinaryList] -> Int -> Criteria
mostCommonInCol xs i =
  let ones = sum $ map (\b -> b !! i) xs
      zeros = sum $ map (\b -> if (b !! i) == 0 then 1 else 0) xs
  in if ones > zeros
     then One
     else
        if zeros > ones
        then Zero
        else Equal
