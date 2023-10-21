printName = do
 fname <- getLine
 lname <- getLine
 putStrLn(fname ++ " " ++ lname)

numSum x y = x + y -- enter them on the same line

numMult = do
    firstLine <- getLine
    secondLine <- getLine
    let firstNum = read firstLine :: Integer
    let secondNum = read secondLine :: Integer
    putStrLn (show (firstNum * secondNum))

circleArea = do
    input <- getLine
    let r = read input :: Float
    putStrLn (show (r * r * pi))

isPointInRectangle = do
    pointAXLine <- getLine
    pointAYLine <- getLine
    pointBXLine <- getLine
    pointBYLine <- getLine
    pointCXLine <- getLine
    pointCYLine <- getLine
    let pointAX = read pointAXLine :: Integer
    let pointAY = read pointAYLine :: Integer
    let pointBX = read pointBXLine :: Integer
    let pointBY = read pointBYLine :: Integer
    let pointCX = read pointCXLine :: Integer
    let pointCY = read pointCYLine :: Integer
    if(pointCX >= pointAX && pointCX <= pointBX)
    then if (pointCY >= pointAY && pointCY <= pointBY)
        then putStrLn "INSIDE"
        else putStrLn "OUTSIDE"
    else putStrLn "OUTSIDE"

addFile = do
    let filePath = "D:/Repos/11/Introduction/file.txt"
    writeFile filePath "This is your new file"
    file <- readFile filePath
    putStrLn file
    appendFile filePath "\nThis is your appended text"
    file <- readFile filePath
    putStrLn file
