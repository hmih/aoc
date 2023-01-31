{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Text.Read as R

data Tree a where
  Leaf :: Eq a => Int -> a -> Tree a
  Node :: Eq a => a -> [Tree a] -> Tree a

deriving instance Show a => Show (Tree a)

data Path a where
  Top :: Path a
  Level :: Eq a => Tree a -> Path a -> Path a

deriving instance Show a => Show (Path a)

data Zipper a = Zipper (Path a) (Tree a) deriving (Show)

-- main :: IO ()
-- main = do
--  input <- I.readFile "in"
--  let split = T.lines input
--      tree = (Node "/" [Leaf 0 "a.txt", Leaf 1 "b.txt", Node "etc" [Leaf 2 "i.log", Leaf 4 "k.log", Node "lib" [Leaf 5 "glibc"]], Node "bin" [Leaf 6 "ls"]])
--      zipper = Zipper Top tree
--  print zipper
--  print $ down zipper
--  print $ right $ down zipper
--  print $ left $ right $ down zipper
--  print $ down $ left $ right $ down zipper
--  print $ up $ down $ left $ right $ down zipper

main :: IO ()
main = do
  input <- I.readFile "in"
  let split = T.lines input
      tree = mkFS split
      zipper = Zipper Top tree
  print tree

mkFS :: [T.Text] -> Tree T.Text
mkFS [] = Leaf 0 ".end"
mkFS (x : xs) =
  let isCD = T.isPrefixOf "$ cd" x
      isLS = x == "$ ls"
      isDir = T.isPrefixOf "dir" x
      isFile = R.decimal (fst $ T.breakOn " " x)
   in if isCD
        then Leaf 1 "hi"
        else Leaf 0 ""

up :: Zipper a -> Zipper a
up (Zipper (Level _ Top) _) = error "hit top, empty"
up (Zipper (Level _ path) _) =
  let dirs = (nodes . leaves) path
      dir = if L.null dirs then error "empty up" else L.head dirs
   in Zipper path dir

down :: Zipper a -> Zipper a
down (Zipper _ (Leaf _ _)) = error "hit leaf, cannot go down"
down (Zipper path curr@(Node _ xs)) =
  let dirs = nodes xs
      dir = if L.null dirs then error "empty down" else L.head dirs
   in Zipper (Level curr path) dir

right :: Zipper a -> Zipper a
right (Zipper path (Node name _)) =
  let dirs = (nodes . leaves) path
      others = L.dropWhile (\(Node n _) -> n /= name) dirs
      dir = (head . tail) others
   in if L.length others < 2 then error "cannot go more right" else Zipper path dir

left :: Zipper a -> Zipper a
left (Zipper path (Node name _)) =
  let dirs = (nodes . leaves) path
      others = L.takeWhile (\(Node n _) -> n /= name) dirs
      dir = L.last others
   in if L.null others then error "cannot go more left" else Zipper path dir

nodes :: [Tree a] -> [Tree a]
nodes xs = [x | x@(Node _ _) <- xs]

leaves :: Path a -> [Tree a]
leaves (Level (Node _ xs) _) = xs
