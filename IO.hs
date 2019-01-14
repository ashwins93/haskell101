main = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly contents =
  let allLines = lines contents
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in  result