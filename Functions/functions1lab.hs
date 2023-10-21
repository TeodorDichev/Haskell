-- problem 1
showPlus s x =
    if null s 
    then show x
    else "(" ++ s ++ "+" ++ (show x) ++ ")"

generateMathExpression list = foldl showPlus "" list

-- problem 2
showPlus' x s = 
    if null s 
    then show x
    else "(" ++ (show x) ++ "+" ++ s ++ ")"

generateMathExpression' list = foldr showPlus' "" list

-- problem 3
compress [] = []
compress x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x

-- problem 4
duplicate list = foldr (\ x xs -> x : x : xs) [] list

-- problem 5
replicateList list n = foldl (\ a b -> a ++ replicate b n) [] list
    where 
        replicate _ 0 = []
        replicate x n = x : replicate x (n - 1)
        
-- problem 6
sliceList list start end = map snd
    $ filter (\ (x,_) -> x >= start && x <= end)
    $ zip [0..] list
                            
sliceList' list start end = drop (start-1) 
    $ take end list