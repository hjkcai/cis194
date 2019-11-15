module Main where

toDigitsRev :: Integer -> [Integer]
toDigitsRev num
  | num <= 0  = []
  | otherwise = (num `mod` 10) : toDigitsRev(num `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

main :: IO ()
main = do
  putStrLn $ show $ toDigitsRev 1234567890
