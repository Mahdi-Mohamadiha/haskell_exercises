-- Answer to question 1
recursiveDivisionList limit = [1 / n | n <- [1 .. limit]]

-- Answer to question 2
recursiveTriangularNumber limit = [((n ^ 2) + n) / 2 | n <- [1 .. limit]]

-- Answer to question 3
let cnt1 = 0
let cnt2 = 0
mergeList (x:xs) (y:ys) = 
    
    

-- Answer to question 4


-- Answer to question 5
quickSort [] = []
quickSort (x : xs) =
    let smallPart = quickSort [a | a <- xs, a <= x]
        bigPart = quickSort [a | a <- xs, a > x]
    in smallPart ++ [x] ++ bigPart