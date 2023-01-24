{-# LANGUAGE OverloadedStrings #-}

module P3 where

import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.Text.Read as R


main :: IO ()
main = do raw <- I.readFile "in"
          let lines = T.lines raw
              x = horizontal lines
              y = vertical lines
          print $ "x: " ++ show x
          print $ "y: " ++ show y
          print $ "x * y: " ++ (show $ x * y)

vertical :: [T.Text] -> Int
vertical xs = if depth > 0 then depth else 0
    where up = foldr (+) 0 $ command "up" xs
          down = foldr (+) 0 $ command "down" xs
          depth = down - up

horizontal :: [T.Text] -> Int
horizontal xs = foldr (+) 0 fs
    where fs = command "forward" xs

command :: T.Text -> [T.Text] -> [Int]
command op xs = map (\l -> if T.isPrefixOf op l then num ((T.splitOn " " l) !! 1) else 0) xs

num :: T.Text -> Int 
num = either (error . show) fst
    . R.signed R.decimal
