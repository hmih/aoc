module Main where

import qualified Data.Array as A
import qualified Data.Char as C
import qualified Data.Containers.ListUtils as U
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified System.IO as I

type Grid = A.Array Int (A.Array Int Int)

-- value over which the cell is set to 0 for the current step
threshold :: Int
threshold = 9

-- iteratively change the grid
main :: IO ()
main = do
  grid <- readInput

  let fold =
        ( \_ -> \g ->
            let g' = step g
                fs = findFlashers g'
             in triggerFlashers g' fs
        )

  let steps :: [Int] = [1 .. 2]
      grid' = foldr fold grid steps

  let tg = testGrid

  print "---"
  printGrid grid
  print "---"
  printGrid grid'
  print "---"
  printGrid tg
  print "---"
  print "end"

-- pretty printing
printGrid :: Grid -> IO ()
printGrid g =
  let rows = A.elems g
      clean = fmap A.elems rows
   in mapM_ print clean

-- single step where all the cells of the grid are incremented by one
step :: Grid -> Grid
step g =
  let (xl, xu) = A.bounds g
      (yl, yu) = A.bounds (g A.! 0)
      xs = [xl .. xu]
      ys = [yl .. yu]
      updated :: [(Int, A.Array Int Int)] =
        fmap
          ( \x ->
              let idx = x
                  val = A.listArray (yl, yu) $ fmap (\y -> (g A.! x A.! y) + 1) ys
               in (idx, val)
          )
          xs
   in g A.// updated

-- triggers cells in the grid which are over the threshold. after the iteration
-- the function looks for new potential flashers and triggers them as well.
-- this happens until no flashers are found
triggerFlashers :: Grid -> [(Int, Int)] -> Grid
triggerFlashers grid [] = grid
triggerFlashers grid octs =
  let grid' =
        foldr
          ( \oct -> \g ->
              let ns = octopusNeighbours g oct
               in updateGrid g ns
          )
          grid
          octs
      further = findFlashers grid'
   in triggerFlashers grid' further

-- find all cells in the grid which have value over the threshold
findFlashers :: Grid -> [(Int, Int)]
findFlashers g =
  let candidates =
        L.concatMap
          ( \(x, r) ->
              fmap
                ( \(y, e) ->
                    if e > threshold
                      then M.Just (x, y)
                      else M.Nothing
                )
                (A.assocs r)
          )
          (A.assocs g)
   in M.catMaybes candidates

-- safely index a point on the grid, if OOB then give the fallback
safePoint :: Grid -> (Int, Int) -> (Int, Int) -> (Int, Int)
safePoint g fallback candidate@(x, y) =
  let (xl, xu) = A.bounds g
      (yl, yu) = A.bounds (g A.! 0)
      ix = if x > xu || x < xl then M.Nothing else M.Just x
      iy = if y > yu || y < yl then M.Nothing else M.Just y
   in if M.isNothing ix || M.isNothing iy then fallback else candidate

-- decides if a point should be incremented or should be set to 0
-- in case a point is at zero, it means it was already triggered this step
-- steps always increment 0s to ones, thus all 0s are set during the current step
incrementPoint :: Grid -> (Int, Int) -> Int
incrementPoint g (x, y) =
  let val = g A.! x A.! y
   in if val == 0
        then 0
        else
          if val > threshold
            then 0
            else val + 1

-- partial update of the grid
updateGrid :: Grid -> [(Int, Int)] -> Grid
updateGrid g [] = g
updateGrid g (point@(x, y) : ps) =
  let v = incrementPoint g point
      a = (g A.! x) A.// [(y, v)]
   in updateGrid (g A.// [(x, a)]) ps

-- get the neighbouring cells plus the cell
octopusNeighbours :: Grid -> (Int, Int) -> [(Int, Int)]
octopusNeighbours g fallback@(x, y) =
  let a = safePoint g fallback (x - 1, y)
      b = safePoint g fallback (x + 1, y)
      l = safePoint g fallback (x, y - 1)
      r = safePoint g fallback (x, y + 1)
      al = safePoint g fallback (x - 1, y - 1)
      ar = safePoint g fallback (x - 1, y + 1)
      bl = safePoint g fallback (x + 1, y - 1)
      br = safePoint g fallback (x + 1, y + 1)
   in U.nubOrd [fallback, a, b, l, r, al, ar, bl, br]

-- helper
strToInts :: String -> [Int]
strToInts s = L.map (C.digitToInt) s

-- load inpu
readInput :: IO Grid
readInput = do
  raw <- I.readFile "in"

  let ls = L.lines raw
      outerLength = (L.length ls) - 1
      innerLength = (L.length (ls !! 0)) - 1
      inners = fmap (\l -> A.listArray (0, innerLength) (strToInts l)) ls
      outers = A.listArray (0, outerLength) inners
   in pure outers

-- desired output
testGrid :: Grid
testGrid =
  A.listArray (0, 9) $
    [ A.listArray (0, 9) [8, 8, 0, 7, 4, 7, 6, 5, 5, 5],
      A.listArray (0, 9) [5, 0, 8, 9, 0, 8, 7, 0, 5, 4],
      A.listArray (0, 9) [8, 5, 9, 7, 8, 8, 9, 6, 0, 8],
      A.listArray (0, 9) [8, 4, 8, 5, 7, 6, 9, 6, 0, 0],
      A.listArray (0, 9) [8, 7, 0, 0, 9, 0, 8, 8, 0, 0],
      A.listArray (0, 9) [6, 6, 0, 0, 0, 8, 8, 9, 8, 9],
      A.listArray (0, 9) [6, 8, 0, 0, 0, 0, 5, 9, 4, 3],
      A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 7, 4, 5, 6],
      A.listArray (0, 9) [9, 0, 0, 0, 0, 0, 0, 8, 7, 6],
      A.listArray (0, 9) [8, 7, 0, 0, 0, 0, 6, 8, 4, 8]
    ]

--
--testGrid1 :: Grid
--testGrid1 =
--  A.listArray (0, 9) $
--    [ A.listArray (0, 9) [6, 5, 9, 4, 2, 5, 4, 3, 3, 4],
--      A.listArray (0, 9) [3, 8, 5, 6, 9, 6, 5, 8, 2, 2],
--      A.listArray (0, 9) [6, 3, 7, 5, 6, 6, 7, 2, 8, 4],
--      A.listArray (0, 9) [7, 2, 5, 2, 4, 4, 7, 2, 5, 7],
--      A.listArray (0, 9) [7, 4, 6, 8, 4, 9, 6, 5, 8, 9],
--      A.listArray (0, 9) [5, 2, 7, 8, 6, 3, 5, 7, 5, 6],
--      A.listArray (0, 9) [3, 2, 8, 7, 9, 5, 2, 8, 3, 2],
--      A.listArray (0, 9) [7, 9, 9, 3, 9, 9, 2, 2, 4, 5],
--      A.listArray (0, 9) [5, 9, 5, 7, 9, 5, 9, 6, 6, 5],
--      A.listArray (0, 9) [6, 3, 9, 4, 8, 6, 2, 6, 3, 7]
--    ]
--
--ptsToGrid :: [((Int, Int), Int)] -> Grid
--ptsToGrid pts =
--  let empty =
--        A.listArray (0, 9) $
--          [ A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
--            A.listArray (0, 9) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
--          ]
--   in foldr
--        ( \((x, y), v) -> \g ->
--            let a = g A.! x
--             in g A.// [(x, a A.// [(y, v)])]
--        )
--        empty
--        pts
--updateGrid :: Grid -> [((Int, Int), Int)] -> Grid
--updateGrid g ps =
--  let updated = ptsToGrid ps
--   in g A.// updated
--
-- updateGrid :: Grid -> [((Int, Int), Int)] -> Grid
-- updateGrid g [] = g
-- updateGrid g (p : ps) =
--   let ((x, y), v) = p
--       a = (g A.! x) A.// [(y, v)]
--    in updateGrid (g A.// [(x, a)]) ps
--
-- [(Int, [(Int, Int)]]
--triggerFlashers :: Grid -> [(Int, Int)] -> Grid
--triggerFlashers grid [] = grid
--triggerFlashers grid octs =
--  let grid' =
--        foldr
--          ( \oct -> \gr ->
--              let ns = octopusNeighbours gr oct
--                  newGrid =
--                    foldr
--                      ( \p@(x, y) -> \g ->
--                          let (_, v) = incrementPoint g p
--                              a = (g A.! x) A.// [(y, v)]
--                           in g A.// [(x, a)]
--                      )
--                      gr
--                      ns
--               in zeroPoint newGrid oct
--          )
--          grid
--          octs
--      blastwave = findFlashers grid'
--   in triggerFlashers grid' blastwave
--
--triggerFlashers :: Grid -> [(Int, Int)] -> Grid
--triggerFlashers grid [] = grid
--triggerFlashers grid octs =
--  let grid' =
--        foldr
--          ( \oct -> \g ->
--              let ns = octopusNeighbours g oct
--                  ps = incrementPoints g ns
--               in updateGrid g ps
--          )
--          grid
--          octs
--      nextStep = findFlashers grid'
--   in triggerFlashers grid' nextStep
--
--incrementPoints :: Grid -> [(Int, Int)] -> [((Int, Int), Int)]
--incrementPoints g ps =
--  fmap
--    ( \idx@(x, y) ->
--        let val = g A.! x A.! y
--         in if val == 0
--              then (idx, 0)
--              else
--                if val > threshold
--                  then (idx, 0)
--                  else (idx, val + 1)
--    )
--    ps
--triggerFlashers :: Grid -> [(Int, Int)] -> Grid
--triggerFlashers grid [] = grid
--triggerFlashers grid octs =
--  let grid' =
--        foldr
--          ( \oct -> \g ->
--              let ns = octopusNeighbours g oct
--               in updateGrid g ns
--          )
--          grid
--          octs
--      further = findFlashers grid'
--   in triggerFlashers grid' further
