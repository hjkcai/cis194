module Main where

-- Exercise 1.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

productMinus2Even :: [Integer] -> Integer
productMinus2Even = product . map (subtract 2) . filter even

-- Exercise 1.2
-- fun2 10 = 10 + fun2 5 = 10 + fun2 16 = 10 + (16 + fun2 8) = 10 + (16 + (8 + fun2 4)) = ...
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

specialSum :: Integer -> Integer
specialSum = sum . filter even . takeWhile (/=0) . iterate it
  where it n
          | n == 1    = 0
          | even n    = n `div` 2
          | otherwise = 3 * n + 1

main :: IO ()
main = return ()
