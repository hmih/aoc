{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Text.Read as R

main :: IO ()
main = do
  raw <- I.readFile "in"
  let split = T.lines raw
  let parts = partition "" split
  let numeric = fmap numify parts
  -- let res = L.maximum $ fmap L.sum numeric
  let res = L.sum $ take 3 $ L.reverse $ L.sort $ fmap L.sum numeric
  print res

partition :: T.Text -> [T.Text] -> [[T.Text]]
partition x xs =
  let (y, ys) = span (\e -> e /= x) xs
   in if (not $ null ys) then [y] ++ (partition x $ tail ys) else []

numify :: [T.Text] -> [Int]
numify xs = fmap (\x -> fst $ E.fromRight (0, "impossible") $ R.decimal x) xs
