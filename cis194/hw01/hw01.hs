validate :: Integer -> Bool
validate num
	| res `mod` 10 == 0 = True
	| otherwise         = False
	where res = sumDigits $ doubleEveryOther $ toDigits num []

-- toDigits should convert positive Integereger s to a list of digits. (For 0 or
-- negative inputs, toDigits should return the empty list.) toDigitsRev
-- should do the same, but with the digits reversed.

toDigits :: Integer -> [Integer] -> [Integer]
toDigits s lst
    | s <= 0     = []
    | s <= 10    = s : lst
    | otherwise = toDigits (s `div` 10) ((s `mod` 10): lst)

-- TODO: I should be using this on validate but my doubleEveryOther is too broken for that
toDigitsRev :: Integer -> [Integer] -> [Integer]
toDigitsRev s lst
    | s <= 10    = lst ++ [s]
    | otherwise = toDigitsRev (s `div` 10) (lst ++ [s `mod` 10])

-- Remember that doubleEveryOther should double every other num-
-- ber beginning from the right, that is, the second-to-last, fourth-to-last, numbers are doubled.
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]

-- Unoptimized but good enough, i guess
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft ls = [if i `mod` 2 /= 0 
                       then (ls !! i) * 2 
                       else ls !! i | i <- indices]
    where indices = [0 .. length ls - 1]

-- The output of doubleEveryOther has a mix of one-digit
-- and two-digit numbers. Define the function
-- sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
	| x > 9     = sum (toDigits x []) + sumDigits xs
	| otherwise = x + sumDigits xs