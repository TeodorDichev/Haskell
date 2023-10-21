-- functions of higher order
basicMathFunc a b func = func a b
mult a b = a * b
add a b = a + b
substr a b = a - b
divide a b = a / b
modNums a b = mod a b

-- map function
listFunc list func = func list
absolute list = map abs list
add1 list = map (1 + ) list

-- filter function
isEven x = x `mod` 2 == 0
removeOdd = filter isEven

-- foldl - left to right
-- foldr - right to left
subtractList list = foldl (-) 0 list
subtractList' list = foldr (-) 0 list

-- zip
zipList list1 list2 = zip list1 list2

-- zipWith
zipListWith list1 list2 func = func list1 list2
inc list1 list2 = zipWith (+) list1 list2
dnc list1 list2 = zipWith (-) list1 list2

zipWithAnonymous list1 list2 = zipWith (\ x y  -> x + y ) list1 list2

-- zad 1
maxFromList list = foldl max (head list) list