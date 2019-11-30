module Main where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

productMinus2Even :: [Integer] -> Integer
productMinus2Even = product . map (subtract 2) . filter even

main :: IO ()
main = return ()
