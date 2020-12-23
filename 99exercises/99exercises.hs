-- Problem 1 - Find the last element of a list. 
myLast :: [a] -> Maybe a
myLast []  = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

-- Problem 2 - Find the last but one element of a list. 
myButLast :: [a] -> Maybe a
myButLast []  = Nothing
myButLast [_] = Nothing
myButLast [x, _] = Just x
myButLast (_:xs) = myButLast xs

-- Problem 3 - Find the K'th element of a list.
-- The first element in the list is number 0. 

elementAt :: [a] -> Int -> a
elementAt list idx = elementAt' list idx 0
    where elementAt' (x:xs) objIdx curIdx  = if objIdx == curIdx 
                                                then x
                                             else elementAt' xs objIdx (curIdx + 1)

-- Problem 4 - Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5 - Reverse a list
myReverse :: [a] -> [a]
myReverse []    = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6 - Check if the list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (myReverse x)

-- Problem 14 - Duplicate the elements of a list

dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15 - Replicate the elements of a list a given number of times

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) reps = repeat x reps ++ repli xs reps
    where repeat elem 1 = [elem]
          repeat elem reps = elem : (repeat elem (reps - 1))