{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as I

data Move = Move Int Int Int deriving (Show)

-- QHPGMHB
main :: IO ()
main = do
  contents <- I.readFile "in"
  let split = T.lines contents
      crates = L.takeWhile (M.isJust . T.find (== '[')) split
      chunked = fmap (T.chunksOf 4) crates
      parts = (fmap . fmap) (T.dropAround (\x -> x == '[' || x == ']') . T.strip) chunked
      encoded = (fmap . fmap) (\x -> if T.null x then Nothing else Just x) parts
      stacks = (fmap M.catMaybes) $ L.transpose encoded
      moves = mkMoves $ tail $ L.dropWhile (not . T.null) split
      result = evalMoves moves stacks
      top = T.concat $ fmap (\x -> if (not . L.null) x then head x else "") result
  print top

evalMoves :: [Move] -> [[T.Text]] -> [[T.Text]]
evalMoves [] xs = xs
evalMoves ((Move n from to) : ms) xs =
  let src = xs !! from
      picked = L.take n src
      src' = L.drop n src

      dst = xs !! to
      -- p1
      -- dst' = (L.reverse picked) ++ dst
      dst' = picked ++ dst

      xs' =
        fmap
          ( \(i, c) ->
              if i == from
                then src'
                else
                  if i == to
                    then dst'
                    else c
          )
          (zip [0 ..] xs)
   in evalMoves ms xs'

mkMoves :: [T.Text] -> [Move]
mkMoves [] = []
mkMoves (x : xs) =
  let parts = T.splitOn " " x
      num = (read . T.unpack) $ parts !! 1
      from = (read . T.unpack) $ parts !! 3
      to = (read . T.unpack) $ parts !! 5
   in (Move num (from - 1) (to - 1)) : mkMoves xs
