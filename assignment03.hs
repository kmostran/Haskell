-- | Kyle Ostrander - 10128524 - CPSC 449 - Assignment 3

-- | Problem 1 - Completed
data Season = Fall | Winter | Summer | Spring
    deriving(Eq,Show)
data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving(Eq,Show)

months :: Season -> (Month, Month, Month)
months Winter = (January,February,March)
months Spring = (April,May,June)
months Summer = (July,August,September)
months Fall = (October,November,December)

-- | Problem 2 - Completed
data Form = And Form Form | Or Form Form | Not Form | Val Bool

eval :: Form -> Bool
eval (And a b) = (eval a) && (eval b)
eval (Or a b) = (eval a) || (eval b)
eval (Not a) = not (eval a)
eval (Val a) = a

-- | Problem 3 - Completed
data NTree = Leaf Int | Node NTree Int NTree

collapse :: NTree -> [Int]
collapse (Leaf a) = [a]
collapse (Node a b c) = (collapse a) ++ [b] ++ (collapse c)

-- | Problem 4 - Completed
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
    deriving Show
    
countLeaves :: PTree a -> Integer
countLeaves PLeaf = 1
countLeaves (PNode n left right) = (countLeaves left) + (countLeaves right)

-- | Problem 5 - Completed
data Store = Empty | Join Int Store Store

maxStore :: Store -> Int
maxStore Empty = 0
maxStore (Join a b c) = maxInt(a,(maxStore b),(maxStore c))

maxInt :: (Int,Int,Int) -> Int
maxInt (a,b,c) | (a>b) && (a>c) = a
               | (b>c) = b
               | otherwise = c

-- | Problem 6 - Completed
data Expr = Num Integer | BinOp Op Expr Expr
    deriving (Eq,Show)
data Op = Add | Mul
    deriving (Eq,Show)

countOp :: Op -> Expr -> Int
countOp a (Num b) = 0
countOp a (BinOp b c d) | (a == b) = 1 + (countOp a c) + (countOp a d)
                        | otherwise = (countOp a c) + (countOp a d)

-- | Problem 7 - Completed
data Tree a = Nil | Value a (Tree a) (Tree a)
    deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Value x left right) = Value (f x) (mapTree f left) (mapTree f right)

countChars :: String -> Integer
countChars x = toInteger (length x)

-- | Problem 8 - Completed
foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree _ y Nil = y
foldTree f y (Value x left right) = foldTree f (f x (foldTree f y right)) left

-- | Problem 9 - Completed
data Road = City String | Fork Road Road
    deriving (Eq, Show)

reachable :: String -> Road -> Bool
reachable a (City b) | a == b = True
                     | otherwise = False
reachable a (Fork b c) = (reachable a b) || (reachable a c)

-- | Problem 10 - Completed
data LR = L | R
    deriving Eq

insertRoad :: (Road,LR) -> String -> Road -> Road
insertRoad (a,b) c (City d) | (c == d) && (b == L) = (Fork a (City d)) --3 Base cases
                            | (c == d) && (b == R) = (Fork (City d) a)
                            | otherwise = (City d)
insertRoad (a,b) c (Fork d e) = (Fork (insertRoad(a,b) c d) (insertRoad(a,b) c e)) -- General Case
                              
-- | Additional Challange
data Instruction = FORW Int | BACKW Int | LEFT | RIGHT
data Direction = North | South | West | East
    deriving Enum

destination :: [Instruction] -> (Int,Int)
destination is = ((destination3 is) !! 0, (destination3 is) !! 1)

destination3 :: [Instruction] -> [Int]
destination3 [] = [0,0]
destination3 (LEFT:xs) = left(zipWith (+) [0,0] (destination3 xs))
destination3 (RIGHT:xs) = right(zipWith (+) [0,0] (destination3 xs))
destination3 ((FORW x):xs) =  zipWith (+) (incrementTuple [0,0] x North) (destination3 xs) 
destination3 ((BACKW x):xs) =  zipWith (+) (incrementTuple [0,0] x South) (destination3 xs)

incrementTuple :: [Int] -> Int -> Direction -> [Int]
incrementTuple xs x North = [0,x] 
incrementTuple xs x East = [x,0] 
incrementTuple xs x South = [0,-x]
incrementTuple xs x West = [-x,0]

right :: [Int] -> [Int]
right xs = [xs !! 1, xs !! 0]

left :: [Int] -> [Int]
left xs = [(xs !! 1)*(-1), xs !! 0]
