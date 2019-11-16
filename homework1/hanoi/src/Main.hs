module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discs from to temp
  | discs == 1 = [(from, to)]
  | discs == 2 = [(from, temp), (from, to), (temp, to)]
  | otherwise  = hanoi (discs - 1) from temp to ++ [(from, to)] ++ hanoi (discs - 1) temp to from

hanoiA :: Integer -> [Move]
hanoiA discs = hanoi discs "a" "b" "c"

main :: IO ()
main = do
  print $ hanoiA 2
  print $ hanoiA 3
  print $ hanoiA 4
