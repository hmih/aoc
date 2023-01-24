{-# LANGUAGE OverloadedStrings #-}

-- https://josephg.com/blog/3-tribes/

module Main where

import Control.Monad.State
import Data.Array (Array, bounds, listArray, (!))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as I

type Board = Array Int (Array Int Int)

type Row = Array Int Int

type Column = Array Int Int

main :: IO ()
main = do
  (calls, boards) <- readInput

  print $ "boards: " ++ show (length boards)
  print $ "calls: " ++ show calls

  let winners = findWinners' calls boards
      (_, _, sl) = (filter (\(s, _, _) -> s == 99) winners) !! 0
      (_, cs, fl) = (filter (\(s, _, _) -> s == 100) winners) !! 0
      lb = (filter (\(b) -> notElem b sl) fl) !! 0

  let final = findAnswer lb cs

  print $ "last board: " ++ show (lb)
  print $ "final: " ++ show final

findAnswer :: Board -> [Int] -> Int
findAnswer b cs =
  let lastCall = last cs
      unmarked = unmarkCalls b cs
      unmarkSum = sum $ fmap (foldr (+) 0) unmarked
   in lastCall * unmarkSum

findWinners' :: [Int] -> [Board] -> [(Int, [Int], [Board])]
findWinners' cs bs =
  let parts = sequentialTake cs
   in map
        ( \p ->
            let ws = map (checkBoard' p) bs
                clean = filter (not . null) $ catMaybes ws
                len = length clean
             in (len, p, clean)
        )
        parts

checkBoard' :: [Int] -> Board -> Maybe Board
checkBoard' cs b =
  let winner = checkRows cs b || checkColumns cs b
   in if winner then Just b else Nothing

readInput :: IO ([Int], [Board])
readInput = do
  raw <- I.readFile "in"
  let (head : lines) = T.lines raw
      calls = map (\c -> (read . T.unpack) c :: Int) $ T.splitOn "," head
      boards = parseBoards $ filter (not . T.null) lines
  return (calls, boards)

unmarkCalls :: Board -> [Int] -> Board
unmarkCalls b cs = fmap (fmap (\e -> if elem e cs then 0 else e)) b

-- [1, 2, 3, 4]
-- [1]
-- [1, 2]
-- [1, 2, 3]
-- [1, 2, 3, 4]
sequentialTake :: [Int] -> [[Int]]
sequentialTake xs = map (\n -> take n xs) [1 .. length xs]

checkRows :: [Int] -> Board -> Bool
checkRows cs b = any (all (\e -> elem e cs)) b

checkColumns :: [Int] -> Board -> Bool
checkColumns cs b =
  let (l, u) = bounds b
      cols = fmap (\c -> fmap (\r -> r ! c) b) [l .. u]
   in any (all (\e -> elem e cs)) cols

parseBoards :: [T.Text] -> [Board]
parseBoards [] = []
parseBoards xs =
  let front = take 5 xs
      back = drop 5 xs
      board = parseBoard front
   in board : parseBoards back

parseBoard :: [T.Text] -> Board
parseBoard xs =
  let clean = map (map T.strip) $ map (filter (not . T.null)) $ map (T.splitOn " ") xs
      intLists :: [[Int]] = map (map (read . T.unpack)) clean
   in listArray (0, 4) $ map (listArray (0, 4)) intLists
