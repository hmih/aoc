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
  Level :: Eq a => [Tree a] -> Path a -> [Tree a] -> Path a

deriving instance Show a => Show (Path a)

data Zipper a = Zipper (Path a) (Tree a) deriving (Show)

main :: IO ()
main = do
  input <- I.readFile "in"
  let split = T.lines input
      tree =
        ( Node
            "/"
            [ Leaf 0 "a.txt",
              Leaf 1 "b.txt",
              Node
                "etc"
                [ Leaf 2 "i.log",
                  Leaf 4 "k.log",
                  Node "lib" [Leaf 5 "glibc"]
                ],
              Node "bin" [Leaf 6 "ls"]
            ]
        )
      zipper = Zipper Top tree
  print zipper
  print $ down zipper
  -- print $ right $ down zipper
  -- print $ left $ right $ down zipper
  -- print $ right $ right $ left $ right $ down zipper
  print $ right $ down zipper
  print $ right $ right $ down zipper
  print $ right $ right $ right $ down zipper
  print $ down $ right $ right $ down zipper
  print $ right $ down $ right $ right $ down zipper
  print $ right $ right $ down $ right $ right $ down zipper
  print $ down $ right $ right $ down $ right $ right $ down zipper
  print $ up $ up $ up $ down $ right $ right $ down $ right $ right $ down zipper

-- print $ up $ down $ right $ right $ left $ right $ down zipper

-- main :: IO ()
-- main = do
--  input <- I.readFile "in"
--  let split = T.lines input
--      tree = mkFS split
--      zipper = Zipper Top tree
--  print tree

-- mkFS :: [T.Text] -> Tree T.Text
-- mkFS [] = Leaf 0 ".end"
-- mkFS (x : xs) =
--  let isCD = T.isPrefixOf "$ cd" x
--      isLS = x == "$ ls"
--      isDir = T.isPrefixOf "dir" x
--      isFile = R.decimal (fst $ T.breakOn " " x)
--   in if isCD
--        then Leaf 1 "hi"
--        else Leaf 0 ""

left :: Zipper a -> Zipper a
left (Zipper Top _) = error "left of top"
left (Zipper (Level [] _ _) _) = error "cannot go more left"
left (Zipper (Level (l : ls) p rs) t) = Zipper (Level ls p (t : rs)) l

right :: Zipper a -> Zipper a
right (Zipper Top _) = error "right of top"
right (Zipper (Level _ _ []) _) = error "cannot go more right"
right (Zipper (Level ls p (r : rs)) t) = Zipper (Level (t : ls) p rs) r

up :: Zipper a -> Zipper a
up (Zipper Top _) = error "up top"
up (Zipper (Level ls p rs) t) =
  let xs = (L.reverse ls) ++ (t : rs)
   in Zipper p (Node (name t) xs)

down :: Zipper a -> Zipper a
down (Zipper _ (Leaf _ _)) = error "down low"
down (Zipper _ (Node _ [])) = error "cannot go down on empty"
down (Zipper p (Node n (x : xs))) = Zipper (Level [] p xs) x

-- nodes :: [Tree a] -> [Tree a]
-- nodes xs = [x | x@(Node _ _) <- xs]
--
-- leaves :: Path a -> [Tree a]
-- leaves (Level (Node _ xs) _) = xs

name :: Tree a -> a
name (Leaf _ x) = x
name (Node x _) = x
