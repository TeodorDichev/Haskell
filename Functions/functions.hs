double x = x + x
square x = x * x
sqr x = sqrt x

isEven x = 
    if mod x 2 == 0 
        then True
        else False

max3 a b c = max (max a b) (max b c)

-- a way with passing a string
execute :: String -> Int -> IO ()
execute func val = 
    if func == "add1"
        then putStrLn (show (val + 1))
        else if func == "remove1"
            then putStrLn (show (val - 1))
            else putStrLn "Unknown function"

-- a way with passing a function
compose func val = putStrLn (show (func val))
add1 val = val + 1
remove1 val = val - 1

pow2 n =
    if n == 0
    then 1
    else 2 * (pow2 (n - 1))

repeatString :: String -> Int -> String
repeatString str n = 
    if n == 0
    then ""
    else str ++ repeatString str (n - 1)


_factorial :: Integer -> Integer
_factorial 0 = 1
_factorial n = n * _factorial (n - 1)

factorial n = putStrLn (show (_factorial n))

_fib :: Integer -> Integer
_fib 1 = 1
_fib 2 = 1
_fib n = _fib (n - 1) + _fib (n - 2)

fibonacci n = putStrLn (show (_fib n))
    