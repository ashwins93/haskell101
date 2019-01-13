data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person { firstName :: String
                    , lastName :: String
                    , age :: Int
                    , height :: Int
                    } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: Num a => (Vector a) -> (Vector a) -> (Vector a)
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: Num a => (Vector a) -> (Vector a) -> (Vector a)
(Vector i j k) `vectMult` (Vector l m n) = Vector (i*l) (j*m) (k*n)

scalarMult :: Num a => (Vector a) -> (Vector a) -> a
(Vector i j k) `scalarMult` (Vector l m n) =  i*l + j*m + k*n

-- Typeclasses
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance (YesNo a) => YesNo (Maybe a) where
  yesno (Just x) = yesno x
  yesno Nothing = False

yesNoIf :: (YesNo y) => y -> a -> a -> a
yesNoIf yesNoVal yesResult noResult = if (yesno yesNoVal) then yesResult else noResult