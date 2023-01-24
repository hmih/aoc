module Main where

import qualified Control.Monad.State as S
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import Debug.Trace (trace)
import qualified System.IO as I

type Stack = [Char]

push :: Stack -> Char -> Stack
push s c = [c] ++ s

pop :: Stack -> (Stack, Maybe Char)
pop [] = ([], Nothing)
pop (x : xs) = (xs, Just x)

closers :: Map.Map Char Char
closers = Map.fromList [('(', ')'), ('<', '>'), ('{', '}'), ('[', ']')]

openers :: Map.Map Char Char
openers = Map.fromList [(')', '('), ('>', '<'), ('}', '{'), (']', '[')]

vals :: Map.Map Char Int
vals = Map.fromList [(')', 3), ('>', 15137), ('}', 1197), (']', 57)]

matching :: Char -> Char
matching x =
  let c = closers
      o = openers
   in if Map.member x c then c Map.! x else o Map.! x

main :: IO ()
main = do
  ls <- readInput

  let input = ls
      parser = (\x -> S.runState (mapM parseLine x) ([], False))
      results = fmap parser input
      sums = fmap (\(vs, _) -> sum vs) results
      total = sum sums

  print sums
  print total

evalState :: Stack -> Char -> (Stack, Int)
evalState stack c =
  let scoreClosing = (\x y -> if (matching x) == y then 0 else (vals Map.! y))
      evalClosing = (\x my -> if (M.isNothing my) then 0 else scoreClosing x (M.fromJust my))
   in if (Map.member c openers)
        then ((push stack c), 0)
        else let (s', my) = (pop stack) in (s', evalClosing c my)

parseLine :: Char -> S.State (Stack, Bool) Int
parseLine c = do
  (stack, flag) <- S.get

  let (stack', v) = if flag == False then evalState stack c else ([], 0)
      flag' = if flag == True || v /= 0 then True else False

  S.put (stack', flag')

  pure v

readInput :: IO [String]
readInput = do
  raw <- I.readFile "in"
  pure $ L.lines raw
