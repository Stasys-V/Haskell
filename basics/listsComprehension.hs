-- Run these commands to compile and launch the file on windows OS
-- ghc -o lists basics/listComprehension.hs
-- .\lists.exe

-- In this file I practice problems with list comprehensions in haskell

-- Function which creates a list of duplicate n elements
myReplicate :: Int -> a -> [a]
myReplicate count var = [var | _ <- [1..count]]

-- Function which returns multiples of k up until n
multiples :: Int -> Int -> [Int]
multiples k n 
  | k == 0    = []
  | otherwise = [x | x <- [1..n], x `mod` k == 0]

-- Function which returns all factors of a number
factor :: Int -> [Int]
factor n = [x | x <- [1..n], n `mod` x == 0]

-- Function which returns a list of perfect numbers up to n
perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum(init (factor x)) == x]

-- Function which returns all numbers divisable by k from a list
divisable :: Int -> [Int] -> [Int]
divisable k list = [x | x <- list, x `mod` k == 0]

-- Function which pairs elements from different lists, then multiplies same index elements and sums everything
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs xy = sum [a * b | (a, b) <- zip xs xy]

-- Function which adds the matching index elements and returns a list
zipAdd :: [Int] -> [Int] -> [Int]
zipAdd xs xy = [a + b | (a, b) <- zip xs xy]

-- Function which retrieves all values associated with a value
find :: Eq a => a -> [(a, b)] -> [b]
find key xs = [snd pair | pair <- xs, fst pair == key]

-- Function which extracts all upper characters from a string
upperChars :: String -> String 
upperChars ss = [x | x <- ss, x >= 'A' && x <= 'Z']

main :: IO()
main = do
    print(myReplicate 5 's')
    print(multiples 4 30)
    print(factor 17, factor 0)
    print(divisable 5 [1,3,5,10,15,7])
    print(perfects 500)
    print(scalarProduct [1, 2, 3] [4, 5, 6])
    print(zipAdd [1, 2, 3] [4, 5, 6])
    print(find 'a' [('a', 5), ('b', 3), ('a', 0), ('z', 19) ])
    print(upperChars "Hello World!")