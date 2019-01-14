import System.Environment

solveRPN :: String -> Float
solveRPN = head . foldl reducer [] . words
  where reducer (x:y:ys) "*" = (x*y):ys
        reducer (x:y:ys) "+" = (x+y):ys
        reducer (x:y:ys) "-" = (y-x):ys
        reducer (x:y:ys) "/" = (y/x):ys
        reducer (x:y:ys) "^" = (y ** x):xs
        reducer (x:xs) "ln" = log x:xs
        reducer xs "sum" = sum xs
        reducer xs numString = read numString:xs

main = do
  ops:_ <- getArgs
  print (solveRPN ops)