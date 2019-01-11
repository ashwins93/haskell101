import Data.List

-- HOC
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x 

collatzSeq :: (Integral a) => a -> [a]
collatzSeq 1 = [1]
collatzSeq x
  | odd x =  x: collatzSeq (x * 3 + 1) 
  | otherwise = x: collatzSeq (x `div` 2)

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

maximum' :: Ord a => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) [] 

sumOfSquares :: Int
sumOfSquares = length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]

search :: Eq a => [a] -> [a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

elemIndex' :: Eq a => a -> [a] -> [Int]
