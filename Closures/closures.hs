-- problem 1
f' a = (\ b -> a * b)

-- problem 2
biggestOf3 a b = (\ c -> 
    if a > b
    then if a > c
        then a
        else c
    else if b > c
        then b
        else c)

-- problem 3
add1 = (\ a -> a + 1)
remove1 = (\ a -> a - 1)

execute a = (\ func -> func a)

-- problem 4
showPlus s = (\ x ->
    if null s
    then show x
    else "(" ++ s ++ "+" ++ (show x) ++ ")"
    )

generateMathExpression list = foldl showPlus "" list

