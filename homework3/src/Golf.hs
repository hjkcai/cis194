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
