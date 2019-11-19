module Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = let l = length xs in map (splitAtR xs) [1..l]

splitAt1 :: [a] -> Int -> ([a], [a])
splitAt1 xs n = splitAt 1 $ snd (splitAt (n - 1) xs)

splitAtR :: [a] -> Int -> [a]
splitAtR [] _ = []
splitAtR xs n = let (x, y) = splitAt1 xs n in x ++ splitAtR y n
