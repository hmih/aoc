module Main

import System as S
import System.File.ReadWrite as F

main : IO ()
main = do
  m <- F.readFile "in"
  case m of
    Left err => S.die $ show err
    Right xs => putStrLn xs
