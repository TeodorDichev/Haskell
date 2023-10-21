-- problem 1
reverseList list = reverseList list
-- or
reverseListRec [] = []
reverseListRec (x:xs) = reverseListRec xs ++ [x]

-- problem 2
listLength [] = 0
listLength list = findLength 1 list
findLength length list = 
    if null list
    then (length - 1)
    else findLength (length + 1) (tail list)

-- problem 3
duplicate [] = []
duplicate (x:xs) =  x : x : duplicate xs

-- problem 4
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list count = (take (count-1) list) ++ dropEvery (drop count list) count

-- problem 5
fibonacci n = findFibonacci n 1 0 1 []
findFibonacci n initialValue prevValue index resultList =
    if index > n 
    then resultList
    else findFibonacci n (initialValue + prevValue) initialValue (index + 1) (resultList ++ [initialValue])
-- or
-- fibonacci n = take n (fib 1 1)
  -- where
    -- fib a b = a : fib b (a + b)

-- problem 6
factorial n = findFactorial n 1 1 []
findFactorial n initialValue index resultList =
    if index > n
    then resultList
    else findFactorial n (initialValue * index) (index + 1) (resultList ++ [initialValue])
-- or
-- factorial n = take n (factorialSeq 1 0)
  -- where
    -- factorialSeq x i = x : factorialSeq (x * (i + 1)) (i + 1)