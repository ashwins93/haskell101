import System.Environment

data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon  =  [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      directA = priceA + a
      crossA = priceB + b + c
      directB = priceB + b
      crossB = priceA + a + c
      shortestA = if directA < crossA then (A, a):pathA else (C, c):(B, b):pathB
      shortestB = if directB < crossB then (B, b):pathB else (C, c):(A, a):pathA
  in  (shortestA, shortestB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
  let (pathA, pathB) = foldl roadStep ([], []) roadSystem
      totalA = sum $ map snd pathA
      totalB = sum $ map snd pathB
  in  if totalA < totalB then reverse pathA else reverse pathB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs: groupsOf n (drop n xs)

main = do
  roads:_ <- getArgs
  let threes = groupsOf 3 (map read $ words roads)
      system = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath system
      pathString = concat $ map (show . fst) path 
      pathPrice = sum $ map snd path
  putStrLn $ "The best path is: " ++ pathString
  putStrLn $ "The price of the path is: " ++ show pathPrice