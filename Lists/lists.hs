-- recursively traversing a lists doubling all values
-- input: doubleList [1,2,3,4,5] output: [2,4,6,8,10]
doubleList list =
    if null list
    then []
    else (2 * (head list) : (doubleList (tail list)))

-- recursively traversing a lists removing odd nums
-- input: doubleList [1,2,3,4,5] output: [2,4]
removeOdd nums = 
    if null nums
    then []
    else
        if (mod (head nums) 2) == 0
        then (head nums) : (removeOdd (tail nums))
        else removeOdd (tail nums)

-- finding the length of a list
-- input: length [1,2,3,4,5] output: 5
listLength [] = 0
listLength list = findLength 1 list
findLength length list = 
    if null list
    then (length - 1)
    else findLength (length + 1) (tail list)

-- creating a list by recursion
-- input: createList 1 10 output: [1,2,3,4,5,6,7,8,9,10]
createList start end = createListLoop [] start end
createListLoop list start end =
    if start > end
    then list
    else createListLoop (list ++ [start]) (start + 1) end -- adds an element to the end of the list

-- creating a reversed list by recursion
-- input: createReverseList 1 10 output: [10,9,8,7,6,5,4,3,2,1]
createReverseList start end = createReverseListLoop [] start end
createReverseListLoop list start end =
    if start > end
    then list
    else createReverseListLoop (start : list) (start + 1) end -- adds an element to the begining of the list


-- creating an endless list
-- input: take 10 ints output:[1,2,3,4,5,6,7,8,9,10]
intsFrom n = n : (intsFrom (n+1))
ints = intsFrom 1

-- returning the n-th elements of the list
nThElement list n = nThElementLoop list (length list) n 0
nThElement [] _ = error "Empty list" 
nThElementLoop list listLength n index =
    if n >= listLength || n < 0
    then error "Index outside bounds of array"
    else if index == n
         then (head list)
         else nThElementLoop (tail list) listLength n (index + 1)

-- another way
--nThElement :: [a] -> Int -> a
--nThElement [] _ = error "Empty list"
--nThElement (x:xs) n
    -- | n < 0 = error "Index outside bounds of array"
    -- | n == 0 = x
    -- | otherwise = nThElement xs (n - 1)

