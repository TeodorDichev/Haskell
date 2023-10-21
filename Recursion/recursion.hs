-- sum numbers tail recursiom
sumNumbers = sumNumbersLoop 0 1
sumNumbersLoop sum index = 
    if index > 10
    then sum
    else (sum + index) + (sumNumbersLoop sum (index + 1))

-- fibonacci tail recursion
fibonacci n = findFibonacci n 1 0 1
findFibonacci n initialValue prevValue index =
    if index >= n 
    then initialValue
    else findFibonacci n (initialValue + prevValue) initialValue (index + 1)

-- factorial tail recursion
factorial n = findFactorial n 1 1
findFactorial n initialValue index =
    if index > n
    then initialValue
    else findFactorial n (initialValue * index) (index + 1)

-- log base 2 way 1
getlog x = round (Prelude.log x)

getlogrec n = 
    if n <= 1
    then 0
    else 1 + getlogrec(n/2)

-- log base 2 way 2
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- Print upside triangle way 1
printTriangle n = putStrLn(triangle n)

triangle :: Integer -> String
triangle n =
    if n > 0
    then (asterixStringRow n "*") ++ "\n" ++(triangle (n - 1))
    else ""

asterixStringRow :: Integer -> String -> String
asterixStringRow n str=
    if n == 0
    then ""
    else str ++ (asterixStringRow (n - 1) str)

-- Print upside triangle way 2
printTriangleVoid 0 = return ()
printTriangleVoid n = 
    do
        putStrLn (asterixStringRowReplicate n)
        printTriangleVoid (n - 1)

asterixStringRowReplicate n = replicate n '*'
