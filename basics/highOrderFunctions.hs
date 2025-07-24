-- Run these commands to compile and launch the file on windows OS
-- ghc -o highOrder basics/highOrderFunctions.hs
-- .\highOrder.exe

-- In this file I learn and implement high order functions which is a core concept in haskell
-- I also work on lambda expressions in more depth for better understanding

-- Function which takes a function with two arguments, and reverses it's arguments
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- Function which takes a binary operator and another function, then applies the function to the two arguments before applying the operator
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f x y = g (f x) (f y)

-- Function map without using the original map
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

-- Fuction map without using the original map, use recursion
myMapR :: (a -> b) -> [a] -> [b]
myMapR _ [] = []
myMapR f (x:xs) = f x : myMapR f xs

-- Function filter without using the original filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x <- xs, f x]

-- Function filter without using the original filter, use recursion
myFilterR :: (a -> Bool) -> [a] -> [a]
myFilterR _ [] = []
myFilterR f (x:xs) 
    | f x = x : myFilterR f xs
    | otherwise = myFilterR f xs

-- Function which counts how many elements satisfy a predicate 
countIf :: (a -> Bool) -> [a] -> Int
countIf _ [] = 0
countIf f (x:xs) 
    | f x = 1 + countIf f xs
    | otherwise = countIf f xs

-- Functions such as: sum, product, and, or, map, length, filter implemented with foldr
mySum :: Num a => [a] -> a 
mySum = foldr (+) 0

myProduct :: Num a => [a] -> a
myProduct = foldr (*) 1

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\x acc -> f x : acc) [] 

myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1 ) 0 

myFilterFoldr :: (a -> Bool) -> [a] -> [a]
myFilterFoldr f = foldr (\x acc -> if f x then x : acc else acc) []

-- Implement foldl from scratch
myFoldl :: (a -> b -> b) -> b -> [a] -> b
myFoldl _ v [] = v
myFoldl f v (x:xs) = myFoldl f (f x v) xs

-- Stimulate fold using foldr 
foldll :: (a -> b -> b) -> b -> [a] -> b
foldll f v xs = foldr (\x g acc -> g (f x acc)) id xs v

-- Implement reverse with foldl
myReverse :: [a] -> [a]
myReverse = foldl (flip(:)) []

-- Function which composes a list of functions and applies them to an argument
composeList :: [a -> a] -> (a -> a)
composeList = foldr (.) id

-- Function which applies a function n times
applyN :: Int -> (a -> a) -> a -> a
applyN n f x 
    | n > 0 = applyN (n - 1) f (f x)
    | otherwise = x


main :: IO()
main = do 

    -- High order function tests
    putStrLn $ show(flip' (-) 5 4) ++ " " ++ show( flip' (-) 4 5)
    putStrLn $ "Is hello and world of equal length? " ++ show (on (==) length "hello" "world")
    putStrLn $ "Sum of 3 and 4 squares is " ++ show(on (+) (^2) 3 4)
    print(on max negate 3 5)
    print(on (&&) even 4 6)
    print(myMap (*2) [1,2,3,4], myMapR (*2) [1,2,3,4])
    print(myFilter (<10) [1..20], myFilterR (<10) [1..20])
    print(countIf even [1,2,3,4,5,6,7,11])
    print(mySum [1..5], sum [1..5])
    print(myProduct [1..5], product [1..5])
    print(myAnd[True,False], myAnd[True,True])
    print(myOr[True, False], myOr[False, False])
    print(myMap2 (*2) [1,2,3,4])
    print(myLength "Hello!")
    print(myFilterFoldr (==2) [1..6], myFilterFoldr odd [1,2,3], myFilterFoldr (/= ' ') "H a s k e l l")
    print(myFoldl (+) 0 [1,2,3,4], foldll (+) 0 [1,2,3,4])
    print(myReverse "abcdef")
    print(composeList [(+2), (*2), (*2)] 2)
    print(applyN 5 (+1) 1)

    
    ----------------------------------------------
    -- Basic lambda expressions
    putStrLn "\nLAMBDA EXPESSIONS \n"
    print( (\x -> x) "This always returns the same argument, which was passed onto it")
    print( (\_ -> 4) "This always returns the same value regardless of what you pass to it")

    -- Returns True if number is even, otherwise false
    print( map (\x -> x `mod` 2 == 0) [1..10] )

    -- Lambda expression which takes a string, then reverses it, then appends the two reversed strings
    print( (\x -> reverse x ++ reverse x )  "string")

    -- Lambda expression which takes two numbers and returns the sum of their squares
    print( (\x y -> x^2 + y^2) 3 4)

    -- Lambda expression whichs takes 3 arguments a predicate x y and returns x if true, y if false
    print( (\f x y -> if f x then x else y ) (>3) 4 2)
    print( (\f x y -> if f x then x else y ) (/=1) 1 2)

    -- Lambda expression which returns a function of addition via currying
    print( (\x -> (\y -> x + y) ) 12 7)

    -- Lambda expression which takes three arguments, apllies the 1st two, returns a function which takes the 3rd argument
    print( (\x y -> (\z -> x + y + z)) 5 13 67)

    -- Lambda expression which filters out unwanted elements
    print( (\f -> filter f ) (<10) [1..25] )

    -- Lambda expression which does operations from the right
    print( (\f xs x -> foldr f x xs ) (-) [1,2,3,4,5] 0 )

    -- Lambda expression which takes two lists, then sum each corresponding element, providing a new list
    print( (\xs ys -> zipWith (+) xs ys ) [1,3,5] [2,4,6]  )

    -- Lambda expression which takes a list of lists and concatenates them. List must have at least 2 elements in order to be concatenated
    print( (\xss -> concat (filter (\xs -> length xs > 1) xss) )  [[0,1],[2],[3,4,5]])

    ------------------------------------------
