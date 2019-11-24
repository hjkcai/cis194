module Golf where

-- Exercise 1
-- The idea of skips is
-- 1. Generate a sequence from 1 to the length of the list meaning the size to skip
-- 2. Map the sequence to `skip`, which is reponsible to generate each of the items of `skips`. In the function `skip`:
-- 2.1. Generate an infinite sequence like [n, 2n, 3n, ...] meaning the indices to take from the list
-- 2.2. Try to take items out of the list instructed by the sequence until there is no more item to take
-- 2.3. Concat the resulting list because `taskAt` returns a list containing only the item to take, or an empty list if the index overflowed
skips :: [a] -> [[a]]
skips xs = map (skip xs) [1..length xs]

skip :: [a] -> Int -> [a]
skip xs n = concat $ takeWhile (not.null) $ map (takeAt xs) [n, 2*n..]

takeAt :: [a] -> Int -> [a]
takeAt xs n = take 1 $ drop (n - 1) xs

-- Exercise 2
-- The idea of `localMaxima` is:
-- 1. Group every 3 elements of the input. e.g. [a, b, c, d] -> [[a, b, c], [b, c, d]]
-- 2. Map each group to their second element if it is the local maximun
-- 3. Concatenate the resulting list
localMaxima :: [Integer] -> [Integer]
localMaxima xs = concatMap toLocalMaximun $ groupBy3 xs

groupBy3 :: [a] -> [[a]]
groupBy3 xs = map (take 3 . (`drop` xs)) [0..length xs - 1]

toLocalMaximun :: Ord t => [t] -> [t]
toLocalMaximun [a,b,c]
  | b > a && b > c = [b]
  | otherwise      = []
toLocalMaximun _   = []

-- Exercise 3
-- It is too hard for me to express my idea in English
-- God bless me
histogram :: [Integer] -> String
histogram xs = unlines $ reverse $ [['0'..'9'], "=========="] ++ stars xs

stars :: [Integer] -> [String]
stars xs = map (map toStar) $ takeWhile (not . all (<=0)) $ iterate (map (subtract 1)) $ countNumbers xs

toStar :: Integer -> Char
toStar n
  | n > 0     = '*'
  | otherwise = ' '

countNumber :: Integer -> [Integer] -> Integer
countNumber n = toInteger . length . filter (==n)

countNumbers :: [Integer] -> [Integer]
countNumbers xs = zipWith countNumber [0..9] (replicate 10 xs)
