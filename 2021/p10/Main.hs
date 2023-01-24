{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  fishes <- getFishes
  print $ V.length $ runFishes fishes 256

--runFishes :: V.Vector Int -> Int -> V.Vector Int
--runFishes v 0 = v
--runFishes v d =
--  let res = V.concatMap (V.fromList . changeFish) v
--   in runFishes res (d - 1)

runFishes :: V.Vector Int -> Int -> V.Vector Int
runFishes v 0 = v
runFishes v d = runFishes (changeFishes v) (d - 1)

changeFishes :: V.Vector Int -> V.Vector Int
changeFishes v = V.modify (\mv -> V.write mv 0 ) v

getFishes :: IO (V.Vector Int)
getFishes = do
  raw <- I.readFile "in"
  (return . V.fromList . fmap length . L.group . L.sort . fmap (\f -> (read $ T.unpack f) :: Int) . T.splitOn ",") raw
