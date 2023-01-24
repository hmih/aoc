{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I

-- too high
-- 5626776931614

main :: IO ()
main = do
  set <- readSet
  let (s, e) = getSetBounds set

  let costs = map (\g -> (g, calculateCost set g)) [s .. e]

  print set
  (print . minimum . map snd) costs
  print "hello, world"

calculateCost :: [(Int, Int)] -> Int -> Integer
calculateCost xs g =
  foldr
    ( \(v, f) acc ->
        let cost = toInteger $ sum [0 .. toInteger $ abs $ (toInteger g) - (toInteger v)]
         in acc + cost * (toInteger f)
    )
    (toInteger 0)
    xs

readSet :: IO [(Int, Int)]
readSet = do
  txt <- I.readFile "in"
  return . map (\g -> (g !! 0, L.length g)) . L.group . L.sort . map (read . T.unpack) $ T.splitOn "," txt

getSetBounds :: [(Int, Int)] -> (Int, Int)
getSetBounds xs =
  let vals = map fst xs
   in (minimum vals, maximum vals)
