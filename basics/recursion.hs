-- Run these commands to compile and launch the file
-- ghc -o recursion basics/recursion.hs
-- .\recursion.exe

-- In this file I practice problems with recursion in haskell


-- Function which computes nth member of fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Function which reverses a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (c:ss) = reverse' ss ++ [c]

-- Function which concatenates two lists, no using ++
append :: [a] -> [a] -> [a]
append [] x = x
append (x:xs) xy = x : append xs xy

-- Function which removes n elements from a list
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' x (c:ss) 
    | x <= 0 = c:ss
    | otherwise = drop' (x-1) ss

-- Function which returns the 1st n elements from a list
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (c:ss)
    | n <= 0 = []
    | otherwise = c : take' (n-1) ss

-- Function which checks if an element is inside a list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (c:ss) 
    | x == c = True
    | otherwise = elem' x ss

-- Function which pairs elements from different lists
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- Function which takes a list and a condition as arguments. It removes all elements up until the condition fails for the first time. 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' condition (c:ss)
    | condition c = dropWhile' condition ss
    | otherwise = c : ss

-- Function which removes duplicates from a list
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (c:ss)
    | elem' c ss = c : nub' (filter (/= c) ss) 
    | otherwise = c : nub' ss

main :: IO()
main = do
    print(fibonacci 1, fibonacci 2, fibonacci 3, fibonacci 4, fibonacci 5, fibonacci 6, fibonacci 7)
    print(reverse' "abcde")
    print(append "Hello" " World!")
    print(drop' 0 "Helllo", drop' 1 "Helllo", drop' 2 "Helllo", drop' (-1) "Helllo", drop' 100 "Helllo")
    print(take' 5 "Something", take' 100 "Something", take' 0 "Something", take' (-2) "Something")
    print(elem' 'c' "Something", elem' '1' "abc123")
    print(zip' ['a', 'b', 'c'] [4, 5, 6], zip' ['a'] [1, 2])
    print(dropWhile' (<10) [1..20], dropWhile' (const False) "List")
    print(nub' "String", nub' "SssS", nub' "sssssss122221233awoiiw")