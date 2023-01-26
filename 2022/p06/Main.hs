{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as I

chunkSize :: Int
-- p1
-- chunkSize = 4
chunkSize = 14

main :: IO ()
main = do
  contents <- I.readFile "in"
  let split = (mkGroups . T.unpack . T.strip) contents
      starts = filter (\(_, x) -> (L.length $ L.nub x) == chunkSize) $ zip [0 ..] split
      first = chunkSize + ((fst . head) starts)
  print first

mkGroups :: String -> [String]
mkGroups [] = []
mkGroups x = [(L.take chunkSize x)] ++ mkGroups (L.drop 1 x)
