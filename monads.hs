import Control.Monad
import Data.Monoid
import Control.Monad.Writer

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs (left + n - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (right + n - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

(|>) :: a -> (a -> b) -> b
x |> f = f x

routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft 1 start
  second <- landRight 2 first
  return second

-- Knight move
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c+2,r+1), (c+2,r-1), (c-2,r+1), (c-2,r-1)
              ,(c+1,r+2), (c+1,r-2), (c-1,r+2), (c-1,r-2)
              ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` (in3 start)

movesIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
movesIn3 start end = do
  first <- moveKnight start
  second <- moveKnight first
  third <- moveKnight second
  guard (third == end)
  return [first, second, third]

-- Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithString :: Writer [String] Int
multWithString = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply the two numbers."]
  return (a*b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList ((++) xs)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
  mempty = DiffList id
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

-- gcd' :: Int -> Int -> Writer (DiffList String) Int
-- gcd' a b
--   | b == 0 = do
--     tell (toDiffList ["Finshed with " ++ show a])
--     return a
--   | otherwise = do
--     tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
--     gcd' b (a `mod` b)
