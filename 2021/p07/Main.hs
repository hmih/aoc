{-# LANGUAGE OverloadedStrings #-}

-- https://josephg.com/blog/3-tribes/

module Main where


import Control.Monad.State
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Data.Array (Array, listArray, bounds, (!))

type Board = Array Int (Array Int Int)
type Row = Array Int Int
type Column = Array Int Int

-- too low
-- 10450

main :: IO ()
main = do
  raw <- I.readFile "in"
  let (head:lines) = T.lines raw
      calls = map (\c -> (read . T.unpack) c :: Int) $ T.splitOn "," head
      boards = parseBoards $ filter (not . T.null) lines

  print $ "boards: " ++ show (length boards)
  print $ "calls: " ++ show calls

  let parts = sequentialTake calls
  let winners = map (\p -> (map (checkBoard p) boards)) parts
      clean = filter (not . null) $ concatMap catMaybes winners
      (winner, call) = clean !! 0

  let unmarked = unmarkCalls winner call
      lastCall = last call
      unmarkSum = sum $ fmap (foldr (+) 0) unmarked
      final = lastCall * unmarkSum

  print $ "winning call: " ++ show call
  print $ "winning board: " ++ show winner
  print $ "last call: " ++ show lastCall
  print $ "unmarked sum: " ++ show unmarkSum
  print $ "final: " ++ show final

unmarkCalls :: Board -> [Int] -> Board
unmarkCalls b cs = fmap (fmap (\e -> if elem e cs then 0 else e)) b

-- [1, 2, 3, 4]
-- [1]
-- [1, 2]
-- [1, 2, 3]
-- [1, 2, 3, 4]
sequentialTake :: [Int] -> [[Int]]
sequentialTake xs = map (\n -> take n xs) [1..length xs]

checkBoard :: [Int] -> Board -> Maybe (Board, [Int])
checkBoard cs b = let
  winner = checkRows cs b || checkColumns cs b
  in if winner then Just (b, cs) else Nothing

checkRows :: [Int] -> Board -> Bool
checkRows cs b = any (all (\e -> elem e cs)) b

checkColumns :: [Int] -> Board -> Bool
checkColumns cs b = let
  (l, u) = bounds b
  cols = fmap (\c -> fmap (\r -> r ! c) b) [l..u]
  in any (all (\e -> elem e cs)) cols

parseBoards :: [T.Text] -> [Board]
parseBoards [] = []
parseBoards xs = let
  front = take 5 xs
  back = drop 5 xs
  board = parseBoard front
  in board : parseBoards back

parseBoard :: [T.Text] -> Board
parseBoard xs = let
  clean = map (map T.strip) $ map (filter (not . T.null)) $ map (T.splitOn " ") xs
  intLists :: [[Int]] = map (map (read . T.unpack)) clean
  in listArray (0, 4) $ map (listArray (0, 4)) intLists
