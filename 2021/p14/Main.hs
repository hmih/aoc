{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array ((!))
import qualified Data.Array as A
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Debug.Trace (trace)

type Grid = A.Array Int (A.Array Int Int)

main :: IO ()
main = do
  grid <- readInput

  let lows = lowestAmongstNeighbours grid
      basins = findBasins grid lows
      toResult = ((foldr (*) 1) . (take 3) . L.reverse . L.sort . (fmap L.length))
      result = toResult basins

  print $ (show $ A.bounds grid) ++ " : " ++ (show $ A.bounds (grid A.! 0))
  print lows
  print result
  print $ T.unpack "hello, world"

-- grid, already selected candidates, point to expand
expandBasin :: Grid -> S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
expandBasin g traversed p =
  let candidates :: S.Set (Int, Int) = expandPoint g p
      forbidden :: S.Set (Int, Int) = S.filter (\(x, y) -> (g ! x ! y) == 9) candidates
      selected :: S.Set (Int, Int) = candidates S.\\ (S.union forbidden traversed)
      base = selected
      rec :: S.Set (S.Set (Int, Int)) =
        S.map
          ( \x ->
              let src = S.fromList [x]
                  traversed' = S.union traversed (base S.\\ src)
               in expandBasin g traversed' x
          )
          base
   in if (L.length base > 1) then (S.union base (S.unions rec)) else base

findBasins :: Grid -> [(Int, Int)] -> [S.Set (Int, Int)]
findBasins g ps =
  let state = S.fromList [(-9, -9)]
   in L.map (\p -> trace (show p) $ expandBasin g state p) ps

expandPoint :: Grid -> (Int, Int) -> S.Set (Int, Int)
expandPoint g (x, y) =
  let fallback = (x, y)
      a = safePoint g fallback (x - 1, y)
      b = safePoint g fallback (x + 1, y)
      l = safePoint g fallback (x, y - 1)
      r = safePoint g fallback (x, y + 1)
   in S.fromList [a, b, l, r]

safePoint :: Grid -> (Int, Int) -> (Int, Int) -> (Int, Int)
safePoint g fallback candidate@(x, y) =
  let (xl, xu) = A.bounds g
      (yl, yu) = A.bounds (g ! 0)
      ix = if x > xu || x < xl then Nothing else Just x
      iy = if y > yu || y < yl then Nothing else Just y
   in if M.isNothing ix || M.isNothing iy then fallback else candidate

-- if either is OOB return max (9), otherwise element
safeIndex :: Grid -> Int -> Int -> Int
safeIndex g x y =
  let (xl, xu) = A.bounds g
      (yl, yu) = A.bounds (g ! 0)
      ix = if x > xu || x < xl then Nothing else Just x
      iy = if y > yu || y < yl then Nothing else Just y
   in if M.isNothing ix || M.isNothing iy then 9 else g ! (M.fromJust ix) ! (M.fromJust iy)

checkNeighbours :: Grid -> Int -> Int -> Bool
checkNeighbours g x y =
  let c = g ! x ! y
      a = safeIndex g (x - 1) y
      b = safeIndex g (x + 1) y
      l = safeIndex g x (y - 1)
      r = safeIndex g x (y + 1)
   in c < a && c < b && c < l && c < r

lowestAmongstNeighbours :: Grid -> [(Int, Int)]
lowestAmongstNeighbours g =
  let rows = A.indices g
      cols = A.indices (g ! 0)
      points :: [(Int, Int)] = L.concatMap (\r -> fmap (\c -> (r, c)) cols) rows
   in filter (\(r, c) -> checkNeighbours g r c) points

readInput :: IO Grid
readInput = do
  raw <- I.readFile "in"

  let strs :: [String] = fmap (T.unpack) (T.lines raw)
      ints :: [[Int]] = fmap (fmap C.digitToInt) strs
      inners :: [A.Array Int Int] = fmap (\l -> A.listArray (0, (L.length l) - 1) l) ints
      grid :: Grid = A.listArray (0, (L.length strs) - 1) inners
   in pure grid
