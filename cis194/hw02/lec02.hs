data FailableDouble = Failure
	            | OK Double
  deriving Show
  
data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

---- 

data Person = Person String Int Thing
	deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

---- // Pattern matching

-- x@pat pattern-matching
-- matches against pattern `pat` but also assigns
-- the name `x` to the entire value being matched
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _ )         = n ++ ", your favorite thing is lame."

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of 
                    Failure -> 0
                    OK d    -> d

-- recursive data types

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x xs) = x * intListProd xs

