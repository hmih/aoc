module Main where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified System.IO as I

type Grid = A.Array Int (A.Array Int Char)

main :: IO ()
main = do
  (grid, instructions) <- readInput

  let grid' =
        foldr
          ( \i -> \g ->
              let v :: Int = (read . drop 2) i
               in if L.isInfixOf "y=" i then foldUp g v else foldLeft g v
          )
          grid
          instructions

  let res = instrLoop grid instructions

  print "---"
  print $ countDots grid'
  print "---"
  mapM_ (printGrid) res
  print "---"
  print "end"

instrLoop :: Grid -> [[Char]] -> [Grid]
instrLoop g [] = [g]
instrLoop g (x : xs) =
  let v :: Int = (read . drop 2) x
      g' = if L.isInfixOf "y=" x then foldUp g v else foldLeft g v
   in g' : (instrLoop g' xs)

countDots :: Grid -> Int
countDots g = L.sum $ L.concatMap (\e -> fmap (\c -> if c == '#' then 1 else 0) (A.elems e)) (A.elems g)

printGrid :: Grid -> IO ()
printGrid g =
  let rows = A.elems g
      clean = fmap A.elems rows
   in mapM_ print clean

emptyGrid :: Int -> Int -> Grid
emptyGrid xb yb =
  let zeros = (repeat '.')
      rows = fmap (\_ -> A.listArray (0, yb) zeros) [0 .. xb]
      full = A.listArray (0, xb) rows
   in full

readInput :: IO (Grid, [String])
readInput = do
  raw <- I.readFile "in"

  let ls = L.lines raw
      grid = extractGrid ls
      instr = extractInstructions ls
   in pure (grid, instr)

merge :: [Char] -> [Char] -> [Char]
merge fold base =
  let fl = L.length fold
      (_, overflow) = L.splitAt fl base
      ol = L.length overflow
      padding = take ol (repeat '.')
      padded = fold ++ padding
   in fmap
        ( \(x, y) ->
            if x == '#' || y == '#'
              then '#'
              else '.'
        )
        (zip base padded)

foldLeft :: Grid -> Int -> Grid
foldLeft grid s =
  let folded =
        fmap
          ( \e ->
              let (b, _f) = L.splitAt s (A.elems e)
                  f = L.reverse $ drop 1 _f
                  v = merge f b
                  vl = L.length v - 1
               in A.listArray (0, vl) v
          )
          (A.elems grid)
      fl = L.length folded - 1
   in A.listArray (0, fl) folded

foldUp :: Grid -> Int -> Grid
foldUp g s =
  let (xl, xu) = A.bounds g
      (yl, yu) = A.bounds (g A.! 0)
      (lh, uh) = ([xl .. (s - 1)], (L.reverse [(s + 1) .. xu]))
      lower = fmap (\x -> A.elems $ g A.! x) lh
      upper = fmap (\x -> A.elems $ g A.! x) uh
      together = zip lower upper
      folded =
        fmap
          ( \(l, u) ->
              fmap
                ( \(x, y) ->
                    if x == '#' || y == '#'
                      then '#'
                      else '.'
                )
                (zip l u)
          )
          together
      inner = fmap (\f -> A.listArray (yl, yu) f) folded
   in A.listArray (0, L.length inner - 1) inner

extractInstructions :: [String] -> [String]
extractInstructions ls = fmap (drop 11) $ filter (L.isInfixOf "fold along") ls

extractGrid :: [String] -> Grid
extractGrid ls =
  let findLineSplit =
        ( \l ->
            let idx = L.findIndex (\c -> c == ',') l
             in if M.isNothing idx
                  then Nothing
                  else Just (l, M.fromJust idx)
        )
      splitIndexes = fmap findLineSplit ls
      together = M.catMaybes splitIndexes
      points :: [(Int, Int)] = fmap (\(l, s) -> let (p1, p2) = L.splitAt s l in (read $ drop 1 p2, read p1)) together
      xbound = (maximum . fmap fst) points
      ybound = (maximum . fmap snd) points
      empty = emptyGrid xbound ybound
      grid =
        foldr
          ( \(x, y) -> \g ->
              let a = (g A.! x)
                  v = a A.// [(y, '#')]
                  f = [(x, v)]
               in g A.// f
          )
          empty
          points
   in grid
