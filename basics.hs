doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x =
  if x < 100
    then x * 2
    else x

boomBangs :: [Int] -> [[Char]]
boomBangs xs =
  [ if x < 10
    then "BOOM"
    else "BANG!"
  | x <- xs
  , odd x
  ]

removeUppercase :: [Char] -> [Char]
removeUppercase str = [c | c <- str, c `elem` ['a' .. 'z']]

rightTriangles :: Int -> Int -> [(Int, Int, Int)]
rightTriangles perimeter max =
  [ (a, b, c)
  | a <- [1 .. max]
  , b <- [1 .. a]
  , c <- [1 .. max]
  , a ^ 2 + b ^ 2 == c ^ 2
  , a + b + c == perimeter
  ]

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

bmiTell :: RealFloat a => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25 = "Normal weight"
  | bmi <= 30 = "Maginally overweight"
  | otherwise = "Obese"
  where
    bmi = weight / height ^ 2

calcBmis :: RealFloat a => [(a, a)] -> [String]
calcBmis xs = [bmiTell w h | (w, h) <- xs]

describeList :: [a] -> String
describeList xs =
  "The list is " ++
  case xs of
    [] -> "empty"
    [x] -> "singleton list"
    xs -> "a longer list"

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y <= x ] ++ [x] ++ quicksort [ y | y <-xs, y > x ]

-- recreating existing functions
length' :: Num b => [a] -> b
-- length' xs = sum [1|_ <- xs]
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 0
product' [x] = x
product' (x:xs) = x * product' xs

maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x
