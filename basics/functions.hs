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