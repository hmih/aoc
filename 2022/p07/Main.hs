{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Text.Read as R

-- import qualified Debug.Trace as T

emptyFile :: Tree T.Text
emptyFile = Leaf "_" 0

emptyDir :: Tree T.Text
emptyDir = Node "_" [emptyFile]

-- customized implementation of "The Zipper" by Gerard Huet
data Tree a where
  Leaf :: (Eq a) => a -> Int -> Tree a
  Node :: (Eq a) => a -> [Tree a] -> Tree a

deriving instance (Show a) => Show (Tree a)

-- Level(l,p,r) contains its list l of elder siblings (starting with the eldest),
-- its father path p, and its list of younger siblings (starting with the youngest).
data Path a where
  Top :: Path a
  Level :: (Eq a) => a -> [Tree a] -> Path a -> [Tree a] -> Path a

deriving instance (Show a) => Show (Path a)

data Zipper a = Zipper {focus :: Tree a, path :: Path a} deriving (Show)

left :: Zipper a -> M.Maybe (Zipper a)
left (Zipper _ Top) = M.Nothing
left (Zipper _ (Level _ [] _ _)) = M.Nothing
left (Zipper t (Level n (l : ls) p rs)) = M.Just $ Zipper l (Level n ls p (t : rs))

right :: Zipper a -> M.Maybe (Zipper a)
right (Zipper _ Top) = M.Nothing
right (Zipper _ (Level _ _ _ [])) = M.Nothing
right (Zipper t (Level n ls p (r : rs))) = M.Just $ Zipper r (Level n (t : ls) p rs)

up :: Zipper a -> M.Maybe (Zipper a)
up (Zipper _ Top) = M.Nothing
up (Zipper t (Level n ls p rs)) = M.Just $ Zipper (Node n (L.reverse ls ++ [t] ++ rs)) p

down :: Zipper a -> M.Maybe (Zipper a)
down (Zipper (Leaf _ _) _) = M.Nothing
down (Zipper (Node _ []) _) = M.Nothing
down (Zipper (Node n (x : xs)) p) = M.Just $ Zipper x (Level n [] p xs)

insertRight :: Zipper a -> Tree a -> Zipper a
insertRight (Zipper _ Top) _ = error "cannot insert right on top"
insertRight (Zipper t (Level n ls p rs)) r = Zipper t (Level n ls p (r : rs))

insertLeft :: Zipper a -> Tree a -> Zipper a
insertLeft (Zipper _ Top) _ = error "cannot insert left on top"
insertLeft (Zipper t (Level n ls p rs)) l = Zipper t (Level n (l : ls) p rs)

insertDown :: Zipper a -> Tree a -> Zipper a
insertDown (Zipper _ Top) _ = error "cannot insert down on top"
insertDown (Zipper (Leaf _ _) _) _ = error "cannot insert down on leaf"
insertDown (Zipper (Node n xs) p) t = Zipper t (Level n [] p xs)

delete :: Zipper a -> Zipper a
delete (Zipper _ Top) = error "cannot delete top"
delete (Zipper _ (Level n ls p (r : rs))) = Zipper r (Level n ls p rs)
delete (Zipper _ (Level n (l : ls) p [])) = Zipper l (Level n ls p [])
delete (Zipper _ (Level n [] p [])) = Zipper (Node n []) (Level n [] p [])

-- puzzle implementation
isDir :: a -> M.Maybe (Zipper a) -> Bool
isDir _ M.Nothing = False
isDir _ (M.Just (Zipper (Leaf _ _) _)) = False
isDir x (M.Just (Zipper (Node n _) _)) = n == x

muntil :: (a -> M.Maybe a) -> a -> [M.Maybe a]
muntil f z = case f z of
  M.Nothing -> []
  m@(M.Just v) -> m : muntil f v

visitDir :: (Show a) => Zipper a -> a -> M.Maybe (Zipper a)
visitDir z x =
  let paths = (muntil left z) ++ (muntil right z)
      zips = L.filter (isDir x) paths
      intoDir = (down . M.fromJust . L.head) zips
   in if L.null zips
        then M.Nothing
        else
          if (L.length zips) == 1
            then intoDir
            else error "many folders"

parseCD :: Zipper T.Text -> T.Text -> Zipper T.Text
parseCD z x =
  let name = T.strip x
      z' = visitDir z name
      err = error $ "cannot go into unknown directory: " ++ show name
   in M.fromJust $ case name of
        ".." -> up z
        _ -> if M.isJust z' then z' else err

addLeaf :: Zipper T.Text -> T.Text -> Zipper T.Text
addLeaf z x =
  let parts = T.splitOn " " x
      disc = parts !! 0
      name = parts !! 1
      num = fst $ E.fromRight (0, "err") (R.decimal disc)
   in case disc of
        "dir" -> insertRight z (Node name [emptyFile])
        _ -> insertRight z (Leaf name num)

parseLS :: Zipper T.Text -> [T.Text] -> (Zipper T.Text, [T.Text])
parseLS z [] = (z, [])
parseLS z xs =
  let contents = L.takeWhile (not . T.isPrefixOf "$ ") xs
      xs' = L.drop (L.length contents) xs
      z' = foldl addLeaf z contents
   in (z', xs')

parseCmd :: Zipper T.Text -> [T.Text] -> (Zipper T.Text, [T.Text])
parseCmd z [] = (z, [])
parseCmd z (x : xs) =
  let cmd = T.take 2 $ T.drop 2 $ T.strip x
      arg = T.strip $ T.drop 5 x
      lsArg = not $ T.null arg
      errArg = error $ "expected naked ls, got " ++ show arg
      errBot = error $ "impossible, found " ++ show x
   in case cmd of
        "cd" -> (parseCD z arg, xs)
        "ls" -> if lsArg then errArg else parseLS z xs
        _ -> errBot

eval :: Zipper T.Text -> [T.Text] -> Zipper T.Text
eval z [] = z
eval z cmds@(x : _) =
  let (z', xs') = parseCmd z cmds
      err = error $ "expected command, got " ++ show x
      res = eval z' xs'
   in if (not $ T.isPrefixOf "$ " x) then err else res

treeSize :: Zipper T.Text -> [(T.Text, Int)]
treeSize z@(Zipper _ Top) = (treeSize . M.fromJust . down) z
treeSize z@(Zipper _ (Level name ls _ rs)) =
  let ts = ls ++ rs
      -- construct faux Node for level, the focus could be a Leaf
      dir = Node name ts
      names = M.mapMaybe justDirs ts
      zips = M.catMaybes $ fmap (visitDir z) names
      rec = L.concatMap treeSize zips
   in [(name, size dir)] ++ rec
  where
    size :: Tree a -> Int
    size (Leaf _ x) = x
    size (Node _ xs) = L.sum $ fmap size xs

    justDirs :: Tree a -> M.Maybe a
    justDirs (Leaf _ _) = M.Nothing
    justDirs (Node n _) = M.Just n

toTop :: Zipper a -> Zipper a
toTop z =
  let x = up z
   in case x of
        M.Nothing -> z
        (M.Just z') -> toTop z'

-- p1
atMost :: Int
atMost = 100000

-- main :: IO ()
-- main = do
--  input <- I.readFile "in"
--  let cmds = tail $ T.lines input
--      -- create top level node
--      z = M.fromJust $ down $ Zipper (Node "/" [emptyFile]) Top
--      z' = eval z cmds
--      top = M.fromJust $ down $ toTop z'
--      size = treeSize top
--      filtered = L.filter (\(_, s) -> s <= atMost) size
--      answer = (L.sum . fmap (\(_, s) -> s)) filtered
--  print size
--  print filtered
--  print answer

-- p2
totalSpace :: Int
totalSpace = 70000000

needAtleast :: Int
needAtleast = 30000000

main :: IO ()
main = do
  input <- I.readFile "in"
  let cmds = tail $ T.lines input
      -- create top level node
      z = M.fromJust $ down $ Zipper (Node "/" [emptyFile]) Top
      z' = eval z cmds
      top = M.fromJust $ down $ toTop z'
      size = treeSize top
      usedSpace = snd $ L.head size
      availableSpace = totalSpace - usedSpace
      toFreeUp = needAtleast - availableSpace
      filtered = L.filter (\(_, s) -> s >= toFreeUp) size
      answer = (L.minimum . fmap (\(_, s) -> s)) filtered
  print size
  print availableSpace
  print toFreeUp
  print answer
