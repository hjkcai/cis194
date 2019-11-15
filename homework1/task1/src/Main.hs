module Main where

toDigitsRev :: Integer -> [Integer]
toDigitsRev num
  | num <= 0  = []
  | otherwise = (num `mod` 10) : toDigitsRev(num `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . double . reverse
  where double [] = []
        double [x] = [x]
        double [x, y] = [x, y * 2]
        double (x:y:xs) = x : (y * 2) : double xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

main :: IO ()
main = do
  putStrLn $ show $ toDigits 12341234
  putStrLn $ show $ (doubleEveryOther . toDigits) 12341234

  putStrLn $ show $ toDigits 1234123
  putStrLn $ show $ (doubleEveryOther . toDigits) 1234123

  putStrLn $ show $ sumDigits [16, 7, 12, 5]
