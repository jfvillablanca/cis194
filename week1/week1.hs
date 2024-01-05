-- Exercise 1
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits num
    | num <= 0 = []
    | otherwise = recmod num []
  where
    recmod x li
        | x == 0 = li
        | otherwise =
            let newX = mod x 10
             in recmod (div x 10) (newX : li)

toDigitsRev num = reverse (toDigits num)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits = reverse (skipEveryOther (* 2) (reverse digits))
  where
    skipEveryOther f [] = []
    skipEveryOther f [x] = x : skipEveryOther f []
    skipEveryOther f (x : y : xs) = x : f y : skipEveryOther f xs
