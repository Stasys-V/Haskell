import Data.Char (toUpper, toLower)

-- Run these commands to compile and launch the file on windows OS
-- ghc -o functions basics/Functions.hs
-- .\functions.exe

-- In this  file I tried to complete a set of very basic problems regarding Haskell to learn it's syntax and get familiarized with it

--Calculate the area of a circle using it's radius
areaDirect :: Double -> Double 
areaDirect x = x ^ 2 * 3.14159

--Calculate the area of a circle by using let
areaLet :: Double -> Double
areaLet x = 
  let pi = 3.14159
  in x ^ 2 * pi

--Calculate the area of a circle by using where
areaWhere :: Double -> Double
areaWhere x = x ^ 2 * pi
  where 
    pi = 3.14159

-- Create a function perfectSquare, which returns a list of all squares up to n
perfectSquare :: Int -> [Int]
perfectSquare n = [x * x | x <- [1..n]]

--Create a function grid n m that returns all coordinate pairs (x,y) where x ∈ [0..n] and y ∈ [0..m]
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

--Create a function square n that returns all coordinate pairs (x,y) where x,y ∈ [0..n] but excluding the diagonal where x == y
square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0..n], y <- [0..n], x /= y]

--factorial that computes n!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1) 

--sumdown that sums numbers from n down to 0
sumDown :: Int -> Int
sumDown 0 = 0
sumDown n = n + sumDown (n - 1)

--power that computes x^n (for non-negative n)
power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n - 1)

--fst3 that returns the first element
fst3 :: (a, b, c) -> a 
fst3 (x, _, _) = x

--snd3 that returns the second element
snd3 :: (a, b, c) -> b 
snd3 (_, x, _) = x

--thd3 that returns the third element
thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

--rotate that converts (x,y,z) to (y,z,x)
rotate :: (a, b, c) -> (b, c, a)
rotate (x, y, z) = (y, z, x)

--Capitalizes the first character
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

--Then write a function title that capitalizes each word in a string (using list comprehensions and your capitalize function).
title :: String -> String
title = unwords . map capitalize . words

--Write a function doubleAll that takes a list of integers and returns a new list with each element doubled, 
--without using any explicit recursion (use other functions to compose this)
doubleAll :: [Int] -> [Int]
doubleAll xs = map double xs
  where double x = 2 * x

--Implement the absolute value function myAbs using a conditional expression
myAbs :: Int -> Int
myAbs x = if x >= 0 then x else -x

-- Write a function safeTail that returns the tail of a list if it's non-empty, otherwise returns an empty list. Use a conditional expression.
safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

--Create a function signClassify that returns a string saying either the number is "positive", "negative", "zero"
signClassify :: Int -> String
signClassify x = if x > 0 then "Positive" else if x == 0 then "Zero" else "Negative"

--Reimplement myAbs using guarded equations.
myAbs2 :: Int -> Int
myAbs2 x 
  | x >= 0 = x
  | otherwise = -x

--Write a function category that takes an age and returns age group
category :: Int -> String
category age 
  | age < 13 = "Child"
  | 13 <= age && age < 20 = "Teenager"
  | otherwise = "Adult"

--Create a guarded version of signClassify
signClassify2 :: Int -> String
signClassify2 number
  | number > 0 = "Positive"
  | number == 0 = "Zero"
  | otherwise = "Negative"

--Implement and (boolean conjunction) as a function myAnd using pattern matching on lists
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

--Write a function addPairs that takes a list of pairs and returns a list of their sums
addPairs :: Num a => [(a, a)] -> [a]
addPairs [] = []
addPairs ((x, y): xs) = (x + y) : addPairs xs

--Implement init (returns all but last element) yourself as myInit using pattern matching. Handle empty list case appropriately.
myInit :: [a] -> [a]
myInit [] = []
myInit [a] = []
myInit (x:xs) = x : myInit xs

--Create a function isSorted that checks if a list is sorted in non-decreasing order using pattern matching.
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted(y:xs)

--Write a function countTrue that counts how many True values are in a list using pattern matching.
countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (True:xs) = 1 + countTrue xs
countTrue (False:xs) = countTrue xs

  -- Use a lambda with foldr to implement the product of a list.
listProduct :: Num a => [a] -> a 
listProduct [] = 1
listProduct xs = foldr ( \x acc -> acc * x) 1 xs

-- Write a function applyTwice that takes a function and a value, and applies the function twice to the value, using a lambda to demonstrate function composition.
applyTwice :: (a -> a) -> a -> a
applyTwice f = \x -> f (f x)

-- Write a function applyOp that takes an operator (as a function), two arguments, and returns the result, 
-- then demonstrate using operator sections to create specialized versions for addition and multiplication.
applyOp :: (a -> a -> a) -> a -> a -> a
applyOp op = op

main :: IO ()
main = do

  print (areaDirect 5)
  print (areaLet 5)
  print (areaWhere 5)
  print (perfectSquare 25)
  print (grid 2 4)
  print (square 5)
  print (factorial 5)
  print (sumDown 5)
  print (power 2 6)
  print (fst3 ("something", 69, 'L'))
  print (snd3 ("something", 69, 'L'))
  print (thd3 ("something", 69, 'L'))
  print (rotate ("something", 69, 'L'))
  print (capitalize "somebODy")
  print (title "today is a sunny, hot day")
  print (doubleAll [5, 4, 3])
  print (myAbs 0)
  print (safeTail [5, 4, 3])
  print (safeTail ([] :: [Int]) )
  print (signClassify 5)
  print (myAbs2 (-6))
  print (category 12)
  print (category 15)
  print (category 30)
  print ( signClassify2 4 , signClassify2 0 , signClassify2 (-2) )
  print (myAnd [True, True] , myAnd [True, False] , myAnd [False, True, False, True])
  print (addPairs [(4,3),(5,6),(9, 0)])
  print (myInit [5, 4, 3], myInit [] :: [Int])
  print (countTrue[], countTrue[True], countTrue[True, False, False, True], countTrue[False])
  print (isSorted [1,2,3], isSorted ([] :: [Int]), isSorted [5,4] )
  ---- Rewrite the expression map (*2) [1,2,3] using a lambda expression instead of the section (*2).
  print (map (\x -> 2 * x) [1, 2, 3])
  print (listProduct [1, 2, 4, 5])
  print (applyTwice (+1) 4)
  print (map (+3) [1,2,3])
  print (filter (<10) [1, 5, 11, 10])
  print (applyOp (-) 7 2, applyOp (*) 4 3)
