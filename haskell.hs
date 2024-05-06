-- Packages
import Data.List (sort)                 -- Used in 8


-- General functions
doubleMe n = n * 2                      -- Used in 4


-- Answer to question 1
recursiveDivisionList limit = [1 / n | n <- [1 .. limit]]


-- Answer to question 2
recursiveTriangularNumber limit = [((n ^ 2) + n) / 2 | n <- [1 .. limit]]


-- Answer to question 3
mergeList [] ys = ys
mergeList xs [] = xs
mergeList (x:xs) (y:ys) =
    if x > y
        then y : mergeList (x:xs) ys
        else x : mergeList xs (y:ys)


-- Answer to question 4
luhnAlgo n = 
    let n2 = doubleMe n
    in
        if n2 > 9
            then n2 - 9
            else n2


-- Answer to question 5
quickSort [] = []
quickSort (x : xs) =
    let smallPart = quickSort [a | a <- xs, a <= x]
        bigPart = quickSort [a | a <- xs, a > x]
    in smallPart ++ [x] ++ bigPart


-- Answer to question 6
isPrime 1 = False
isPrime 2 = True
isPrime n
    | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
    | otherwise = True

primeNumbers n = filter isPrime [2..n]


-- Answer to question 7
sum' nestedList = 
    let numberList = concat nestedList
    in sum numberList


-- Answer to question 8
flatten' nestedList = 
    let numberList = concat nestedList
    in sort numberList
