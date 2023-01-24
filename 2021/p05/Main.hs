{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Base (divInt)
import Data.Bits (shiftL)
import qualified Data.Text as T
import qualified Data.Text.IO as I

type BinaryNumber = T.Text


-- 12970424 too high
-- 3242606

main :: IO ()
main = do
  raw <- I.readFile "in"
  let lines = T.lines raw
      lineLength = T.length $ lines !! 0
      fileSize = length lines
      halfSize = divInt fileSize 2

  if (all (\l -> T.length l == lineLength) lines) then print "ok" else error "bad"

  let cols = [0..(lineLength - 1)]
      extracted = map (\i -> extractCol lines i) cols

  let gamma = binaryToDecimal $ map (\c -> mcb c halfSize) extracted
  let epsilon = binaryToDecimal $ map (\c -> lcb c halfSize) extracted
  let power = gamma * epsilon

  print $ "gamma: " ++ show gamma
  print $ "epsilon: " ++ show epsilon
  print $ "power: " ++ show power

mcb :: [Int] -> Int -> Int
mcb xs half =
  let c = sum xs
  in if c > half then 1 else
     if c < half then 0 else
     error "equal number"

lcb :: [Int] -> Int -> Int
lcb xs half = if mcb xs half == 1 then 0 else 1

binaryToDecimal :: [Int] -> Int
binaryToDecimal b = sum $ map (\(f, v) -> f * v) $ zip (reverse b) (map (\i -> shiftL 1 i) [0..])

extractCol :: [BinaryNumber] -> Int -> [Int]
extractCol xs i = map (\x -> (read $ [T.index x i]) :: Int) xs
