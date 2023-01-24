{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Text.Read as R

data Command = Forward | Up | Down
  deriving (Eq, Show, Read)

type Entry = (Command, Int)

type Aim = Int
type Movement = Int
type Depth = Int
type Travel = (Aim, Movement, Depth)

-- wrong, too low
-- "a: 955"
-- "f: 2083"
-- "d: 271679421"
--
-- wrong, too high
-- "a: 955"
-- "m: 2083"
-- "d: 271679421"
-- "m * d: 565908233943"
-- wrong, too high
-- 563819059931


main :: IO ()
main = do raw <- I.readFile "in"
          let lines = T.lines raw
              words = map T.words lines
              entries = map toEntry words
              startState = (0, 0, 0)
              (a, m, d) = execState (explore entries) startState
          print $ "a: " ++ show a
          print $ "m: " ++ show m
          print $ "d: " ++ show d
          print $ "m * d: " ++ show (m * d)

explore :: [Entry] -> State Travel Depth
explore [] = do
  (pa, pm, pd) <- get
  return pd

explore ((c, n):xs) = do
  (pa, pm, pd) <- get
  case c of
    Forward ->
      let fwd = pm + n
          depth = pd + pa * n
      in put (pa, fwd, depth)
    Up -> put (pa - n, pm, pd)
    Down -> put (pa + n, pm, pd)
  explore xs

toEntry :: [T.Text] -> Entry
toEntry [op, val] = (toCommand op, (read $ T.unpack val) :: Int)
toEntry _ = error "invalid format"

toCommand :: T.Text -> Command
toCommand "forward" = Forward
toCommand "down" = Down
toCommand "up" = Up
