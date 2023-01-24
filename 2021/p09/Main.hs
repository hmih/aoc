{-# LANGUAGE OverloadedStrings #-}

module Main where

-- too high
-- 20553

import Data.Array ((!))
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Debug.Trace (trace)
import GHC.Conc (numCapabilities)

data Coordinate = Coordinate
  { x :: Int,
    y :: Int
  }
  deriving (Show, Ord, Eq, Read)

data Line = Line
  { start :: Coordinate,
    end :: Coordinate
  }
  deriving (Show, Eq, Read)

main :: IO ()
main = do
  raw <- readInput

  let lines = parseLines raw
      bounds = diagramBounds lines

  print $ "bounds: " ++ show bounds

  let overlaps = findOverlaps lines

  print $ overlaps

findOverlaps :: [Line] -> Int
findOverlaps xs =
  let coordinates = fillInLines xs
      uniques = S.toList $ S.fromList coordinates
      timesFound = map (numTimesFound coordinates) uniques
   in length $ filter ((>= 2)) timesFound

numTimesFound :: [Coordinate] -> Coordinate -> Int
numTimesFound xs x = (length . filter (== x)) xs

buildLine :: Int -> Int -> Int -> Int -> Line
buildLine sx sy ex ey = Line {start = Coordinate sx sy, end = Coordinate ex ey}

fillInLines :: [Line] -> [Coordinate]
fillInLines xs = concatMap fillInLine xs

-- TODO: diagonals
fillInLine :: Line -> [Coordinate]
fillInLine l =
  let sx = (x . start) l
      sy = (y . start) l
      ex = (x . end) l
      ey = (y . end) l
   in if sx == ex
        then map (\y -> Coordinate sx y) [(min sy ey) .. (max sy ey)]
        else
          if sy == ey
            then map (\x -> Coordinate x sy) [(min sx ex) .. (max sx ex)]
            else fillInDiagonal l

fillInDiagonal :: Line -> [Coordinate]
fillInDiagonal l =
  let sx = (x . start) l
      sy = (y . start) l
      ex = (x . end) l
      ey = (y . end) l
   in map (\(x, y) -> Coordinate x y) $
        if sx > ex
          then
            if sy > ey
              then zip (reverse [ex .. sx]) (reverse [ey .. sy])
              else zip (reverse [ex .. sx]) [sy .. ey]
          else
            if sy > ey
              then zip [sx .. ex] (reverse [ey .. sy])
              else zip [sx .. ex] [sy .. ey]

emptyList :: Int -> [Int]
emptyList x = take x (repeat 0)

parseLines :: [T.Text] -> [Line]
parseLines xs = map (\x -> fromParts $ (T.splitOn " -> ") $ T.strip x) xs

lineOverlaps :: [Line] -> [(Line, [Coordinate])]
lineOverlaps (x : xs) = [(x, [])]

fromParts :: [T.Text] -> Line
fromParts [s, e] =
  let sp :: [Int] = map (read . T.unpack) $ T.splitOn "," s
      ep :: [Int] = map (read . T.unpack) $ T.splitOn "," e
      start = Coordinate (sp !! 0) (sp !! 1)
      end = Coordinate (ep !! 0) (ep !! 1)
   in Line {start = start, end = end}

diagramBounds :: [Line] -> (Int, Int)
diagramBounds xs =
  let maxX = maximum $ concatMap (\l -> [(x . start) l, (x . end) l]) xs
      maxY = maximum $ concatMap (\l -> [(y . start) l, (y . end) l]) xs
   in (maxX, maxY)

readInput :: IO [T.Text]
readInput = do
  lines <- I.readFile "in"
  return $ T.lines lines
