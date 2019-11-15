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

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

main :: IO ()
main = do
  putStrLn $ "validate 4012888888881881 = " ++ (show $ validate 4012888888881881)
  print $ toDigits 4012888888881881
  print $ (doubleEveryOther . toDigits) 4012888888881881
  print $ (sumDigits . doubleEveryOther . toDigits) 4012888888881881

  putStrLn $ "validate 4012888888881882 = " ++ (show $ validate 4012888888881882)
  print $ toDigits 4012888888881882
  print $ (doubleEveryOther . toDigits) 4012888888881882
  print $ (sumDigits . doubleEveryOther . toDigits) 4012888888881882
