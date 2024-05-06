-- Packages
import Data.Char (digitToInt)           -- Used in 1
import Data.List (nub)                  -- Used in 7
import Data.List ((\\))                 -- Used in 7


-- General functions
doubleMe n = n * 2                      -- Used in 1
convertToList str = map digitToInt str  -- Used in 1
listMaker n = [2 .. n]                  -- Used in 4


-- Answer to question 1
luhnAlgo n = 
    let n2 = doubleMe n
    in
        if n2 > 9
            then False
            else True

luhnCheck inputList = all luhnAlgo (convertToList inputList)  -- input must be sring like "123456"


-- Answer to question 2
filterF p = foldr (\x acc -> if p x then x : acc else acc) []


-- Answer to question 3
sum' Nothing _ = Nothing
sum' _ Nothing = Nothing
sum' (Just x) (Just y) = Just (x + y)  -- try sum' Nothing (Just 4)


-- Answer to question 4
foldFactorial n = 
    let xs = listMaker n
    in
        foldl (\acc x -> acc * x) 1 xs


-- Answer to question 5
customFunction xs y = filter (\(_, len) -> len > y) (zip xs (map length xs))


-- Answer to question 6
data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)
    deriving Show

countNodes Leaf = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

countLeaves Leaf = 1
countLeaves (Node _ left right) = countLeaves left + countLeaves right

rightNodeLeaf Leaf = 0
rightNodeLeaf (Node _ left right) = 1 + rightNodeLeaf right

sampleTree =
    Node 1
        (Node 2
            (Node 4 Leaf Leaf)
            (Node 5 Leaf Leaf))
        (Node 3 Leaf Leaf)


-- Answer to question 7
omissionsList [] _ = []
omissionsList (base:baseList) limit = [base * n | n <- [2 .. (limit `div` 2)], base * n < limit] : omissionsList baseList limit

sieveOfEratosthenes n =
    let rrd = floor (sqrt (fromIntegral n))
        primaryList = listMaker rrd
        secondaryList = [2 .. (n-1)]
    in
        secondaryList \\ (nub (concat (omissionsList primaryList n)))
