-- | Kyle Ostrander - 10128524 - CPSC 449 - Assignment 2
-- | Imports
import Data.Char
import Data.List

-- | Problem 1 - Completed
productFactorial :: Int -> Int 
productFactorial 0 = 1  
productFactorial n = factorial(n) * productFactorial(n-1)
    where factorial n | n == 0 = 1
                      | otherwise = n * factorial(n-1)

-- | Problem 2 - Completed
smallestFactor :: Int -> Int
smallestFactor n | n == 0 = 0
                 | length(factors(n)) == 1 = head(factors(n))
                 | otherwise = factors(n) !! 1
    where factors n = [x | x <- [1..n], mod n x == 0]

-- | Problem 3 - Completed
gameOddEven :: Int -> [Int]
gameOddEven n | n == 1 = [n]
              | mod n 2 == 0 = [n] ++ gameOddEven(div n 2)
              | otherwise = [n] ++ gameOddEven(n * 3 + 1)

-- | Problem 4 - Completed
isGoodPassword :: String -> Bool
isGoodPassword pw | (length(pw) >= 8) && hasLetters(pw) && hasDigits(pw) && hasUpper(pw) && hasLower(pw) = True
                  | otherwise = False

hasDigits :: [Char] -> Bool
hasDigits [] = False
hasDigits (x:xs) = isDigit x || hasDigits xs

hasLetters :: [Char] -> Bool
hasLetters [] = False
hasLetters (x:xs) = isLetter x || hasLetters xs

hasUpper :: [Char] -> Bool
hasUpper [] = False
hasUpper (x:xs) = isUpper x || hasUpper xs

hasLower :: [Char] -> Bool
hasLower [] = False
hasLower (x:xs) = isLower x || hasLower xs

-- | Problem 5 - Completed
isPrime :: Int -> Bool
isPrime n | n == 1 = False 
          | length(factors(n)) > 2 = False
          | otherwise = True
    where factors n = [x | x <- [1..n], mod n x == 0]

-- | Problem 6 - Completed
allDivisors :: Int -> [Int]
allDivisors n = [x | x <- [1..n], mod n x == 0]

-- | Problem 7 - Completed
matches::Int->[Int]->[Int]
matches x [] = []
matches x (y:ys) | x==y = x:(matches x ys)
                 | otherwise = matches x ys

-- | Problem 8 - Completed
solveQuadraticEquation :: Double -> Double -> Double -> (Double, Double)
solveQuadraticEquation a b c = (x, y)
    where 
        x = (-b + discriminant) / (2*a)
        y = (-b - discriminant) / (2*a)
        discriminant = sqrt ((b ** 2) - (4 * a * c))

-- | Problem 9 - Completed
occursIn  :: Int -> [Int] -> Bool
occursIn  x xs = elem x xs 

-- | Problem 10 - Completed
allOccurIn :: [Int] -> [Int] -> Bool
allOccurIn []y = True
allOccurIn (x:xs) y = elem x y && allOccurIn xs y 

-- | Problem 11 - Completed
sameElements :: [Int] -> [Int] -> Bool
sameElements xs ys = (allOccurIn xs ys) && (allOccurIn ys xs)

-- | Problem 12 - Completed
numOccurrences :: Int -> [Int] -> Int
numOccurrences key [] = 0
numOccurrences key (x:xs) | key == x = 1 + numOccurrences key xs
                         | otherwise = numOccurrences key xs

-- | Problem 13 - Completed
allUrls :: String -> [String]
allUrls [] = []
allUrls xs | isPrefixOf "http://" xs = [getUrl (deleteN 0 xs)] ++ allUrls (deleteN 0 xs)
           | otherwise = [] ++ allUrls (deleteN 0 xs)

getUrl :: String -> String
getUrl xs | isInfixOf "http://" xs = getUrlTrim xs
          | otherwise = ['h'] ++ xs

getUrlTrim :: String -> String
getUrlTrim xs | isInfixOf "http://" xs = getUrlTrim (deleteN (length(xs)-1) xs)
              | otherwise = ['h'] ++ reverseList(lastN (length(xs)-6) (reverseList xs))
              
lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

deleteN :: Int -> [a] -> [a]
deleteN _ [] = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- | Problem 14 - Completed
-- | Iterates through the list
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = [x] ++ sieve(sieveEliminate x xs)

-- | Eliminates non prime numbers in the list
sieveEliminate :: Int -> [Int] -> [Int]
sieveEliminate factor [] = []
sieveEliminate factor (x : xs) | mod x factor == 0 = sieveEliminate factor xs
                               | otherwise = [x] ++ sieveEliminate factor xs

-- | Problem 15 - Completed
-- | Main function - deal with base cases and call the recursive function
pascal :: Int -> [Int]
pascal n | n == 1 = [1]
         | n == 2 = [1,1]
         | otherwise = [doRows n 0] ++ pascalHelper n 1

-- | Recursive function calculates the row         
pascalHelper :: Int -> Int -> [Int]
pascalHelper row column | row == (column) = [1]
                        | otherwise = [doRows row column] ++ pascalHelper row (column+1)

-- | Recursive function calculates the row entries
doRows :: Int -> Int -> Int
doRows row column | column == 0 || column == row = 1
                  | otherwise = doRows (row-1) (column-1) + doRows (row-1) column
