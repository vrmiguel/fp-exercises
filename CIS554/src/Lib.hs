-- module Lib
--     ( CIS554
--     ) where

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

-- Returns True if the element occurs in the list. 
member :: Eq a => a -> [a] -> Bool
member _ [] = False
member elem (x:xs) 
	| x == elem = True
	| otherwise = member elem xs

-- Counts the number of times a value occurs in a list. 
count :: (Num n, Eq a) => a -> [a] -> n
count _ [] = 0
count y (x:xs) = if x == y 
				 then (count y xs) + 1 
				 else count y xs

-- Tests whether all elements of a list satisfy a given condition.
forall :: (a -> Bool) -> [a] -> Bool
forall _ [] = True
forall test (x:xs) = if test x
						then forall test xs
						else False

-- Tests whether any element of a list satisfies a given condition.
exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists test (x:xs) = if test x
						then True
						else exists test xs

-- Finds the first element of a list that satisfies a given condition.
first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first test (x:xs) = if test x
						then Just x
						else first test xs
-- tests:
	-- first (\x -> mod x 2 == 0) [3, 6, 5, 2]

single' :: (a -> Bool) -> [a] -> Int -> Bool
single' _ [] trues = trues == 1
single' test (x:xs) trues
	| trues > 1 = False
	| test x    = single' test xs (trues + 1)
	| otherwise = single' test xs trues

-- Tests whether exactly one element of a list satisfies a given condition.
single :: (a -> Bool) -> [a] -> Bool
single test list = single' test list 0


-- majority' :: Eq a => [a] -> a -> Int -> Maybe a
-- majority' (x:xs) currentCandidate counter
-- 	| counter == 0          = majority' (x:xs) x 1
-- 	| x == currentCandidate = majority' xs x (counter + 1)
-- 	| otherwise             = majority' xs x (counter - 1)


-- -- Find, in linear time, the majority element in a list (the element that occurs more than half the time), or Nothing if there is no majority element.
-- majority :: Eq a => [a] -> Maybe a
-- majority (x:xs) = majority' (x:xs) x 0

-- Make one step in the Collatz sequence. Collatz1 of 1 is 1; collatz1 of an even 
-- number is half that number; and collatz1 of an odd number is three times the number plus one. 
collatz1 :: Int -> Int
collatz1 1 = 1
collatz1 x = if mod x 2 == 0
				then div x 2
				else 3 * x + 1

-- Collatz of 1 is 1; collatz of any other number is the collatz of the collatz1 of that number. 
collatz :: Int -> Int
collatz 1 = 1
collatz x = collatz (collatz1 x)