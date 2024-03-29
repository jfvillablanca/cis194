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

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits numList = sum (expandDigits numList)
  where
    expandDigits [] = []
    expandDigits (x : xs) = toDigits x ++ expandDigits xs

-- Exercise 4
validate creditCardNumber
    | evaluate creditCardNumber `mod` 10 == 0 = True
    | otherwise = False
  where
    evaluate cc = sumDigits (doubleEveryOther (toDigits cc))
