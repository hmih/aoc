{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Vector.Storable as V
import Foreign.C.Types (CInt)
import qualified Numeric.LinearAlgebra.Data as M
import qualified Numeric.LinearAlgebra.Devel as D

directions :: M.Matrix CInt -> (Int, Int) -> [V.Vector CInt]
directions m (rix, cix) =
  let row = M.flatten $ m M.? [rix]
      col = M.flatten $ M.tr $ m M.¿ [cix]
      -- slice off vectors
      up = V.take (rix + 1) col
      down = V.drop rix col
      left = V.take (cix + 1) row
      right = V.drop cix row
   in [up, V.reverse down, left, V.reverse right]

-- p1
-- isVisible :: M.Matrix CInt -> (Int, Int) -> Bool
-- isVisible m ix =
--  -- pivot in back
--  let dirs = directions m ix
--      corner = L.any (== True) $ fmap (\v -> V.length v == 1) dirs
--      tallest = L.any (== True) $ fmap (\v -> V.maxIndex v == V.length v - 1) dirs
--   in corner || tallest

-- p1
-- main :: IO ()
-- main = do
--   input <- I.readFile "in"
--   let split = fmap T.unpack $ T.lines input
--       vals = (fmap . fmap) (D.fi . C.digitToInt) split
--       matrix = M.fromLists vals
--       idxs = [[(x, y) | y <- [0 .. (M.cols matrix) - 1]] | x <- [0 .. (M.rows matrix) - 1]]
--       visibles = (fmap . fmap) (isVisible matrix) idxs
--       answer = L.length $ L.filter (== True) $ L.concat visibles
--   print matrix
--   -- mapM_ print visibles
--   print answer

-- p2
scenicScore :: M.Matrix CInt -> (Int, Int) -> Int
scenicScore m ix =
  let dirs = directions m ix
      revd = fmap V.reverse dirs
   in L.product $ fmap score revd
  where
    score :: V.Vector CInt -> Int
    score x =
      let len = V.length x
          h = V.head x
          t = V.tail x
          seen = V.takeWhile (h >) t
          visible = if V.length seen < V.length t then V.length seen + 1 else V.length seen
       in if len == 1 then 0 else visible

--
-- -- p2
main :: IO ()
main = do
  input <- I.readFile "in"
  let split = fmap T.unpack $ T.lines input
      vals = (fmap . fmap) (D.fi . C.digitToInt) split
      matrix = M.fromLists vals
      idxs = [[(x, y) | y <- [0 .. (M.cols matrix) - 1]] | x <- [0 .. (M.rows matrix) - 1]]
      visibles = (fmap . fmap) (scenicScore matrix) idxs
      answer = L.maximum $ L.concat visibles
  print matrix
  -- mapM_ print visibles
  print answer
