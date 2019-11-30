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

-- Exercise 2
data Tree a = Leaf
            | Node {- height -} Integer
                     {- left -} (Tree a)
                    {- value -} a
                    {- right -} (Tree a)
            deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree _ = Leaf   -- Let's skip this exercise for now

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr f False
  where f x False = x
        f x True = not x

map' :: (a -> b) -> [a] -> [b]
map' f = foldr g ([] :: [b])
  where g a l = f a : l

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z = foldr (flip f) z . reverse

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : map ((+1) . (*2)) (filter (`notElem` sieves) [1..n])
  where sieves = map (uncurry f) $ filter (uncurry (<=)) $ cartProd [1..n] [1..n]
        f i j  = i + j + 2 * i * j

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

main :: IO ()
main = return ()
