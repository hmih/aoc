module Main where

import qualified Data.Char as C
import qualified Data.Containers.ListUtils as U
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import qualified System.IO as I

type AdjecencyList = Map.Map String [String]

main :: IO ()
main = do
  al <- readInput

  print "--"
  print al
  print "--"

  let paths = bfs al "start" ["start"]

  print paths
  print $ L.length paths

readInput :: IO AdjecencyList
readInput = do
  raw <- I.readFile "in"

  let edges = L.lines raw
      splitPoints = fmap (\e -> let idx = L.findIndex (\c -> c == '-') e in M.fromJust idx) edges
      paired = (zip edges splitPoints)
      points = fmap (\(e, s) -> let (p1, p2) = L.splitAt s e in (p1, drop 1 p2)) paired
      assocs = L.concatMap (\(p1, p2) -> [(p1, p2), (p2, p1)]) points
      grouped = (L.groupBy (\f s -> fst f == fst s) . L.sort) assocs
      adjacents = fmap (\g -> let root = (fst . L.head) g in (root, fmap snd g)) grouped
      adjacency = Map.fromList adjacents
   in pure adjacency

bfs :: AdjecencyList -> String -> [String] -> [[String]]
bfs _ "end" visited = [visited]
bfs al current visited =
  let neighbours = M.fromJust $ Map.lookup current al
      unvisited = filter (canTraverse visited) neighbours
   in L.concatMap (\v -> bfs al v (visited ++ [v])) unvisited

canTraverse :: [String] -> String -> Bool
canTraverse visited v = L.notElem v visited || L.all C.isUpper v || canVisitSmallCaveTwice visited v

canVisitSmallCaveTwice :: [String] -> String -> Bool
canVisitSmallCaveTwice visited v =
  let lc = filter (all C.isLower) visited
      cleaned = (L.length . U.nubOrd) lc
      size = L.length lc
   in cleaned == size && v /= "start"
