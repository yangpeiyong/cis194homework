-- homework

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse.toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | length(xs) `mod` 2 == 0 = (x:doubleEveryOther xs)
  | otherwise = (x*2:doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x<10 = x + sumDigits xs
  | otherwise = (x `mod` 10) + (x `div` 10) + (sumDigits xs)

validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0


