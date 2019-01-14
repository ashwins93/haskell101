import System.Directory
import System.IO
import System.Environment
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = 
  [ ("add", add)
  , ("view", view)
  , ("remove", remove)
  , ("bump", bump)
  ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [filePath, todo] = appendFile filePath (todo ++ "\n")

view :: [String] -> IO ()
view [filePath] = do
  contents <- readFile filePath
  let tasks = lines contents
      numbered = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
  mapM putStrLn numbered
  return ()

remove :: [String] -> IO ()
remove [filePath, numString] = do
  handle <- openFile filePath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numString
      tasks = lines contents
      filtered = delete (tasks !! number) tasks
  hPutStr tempHandle $ unlines filtered
  hClose handle
  hClose tempHandle
  removeFile filePath
  renameFile tempName filePath

bump :: [String] -> IO ()
bump [filePath, numString] = do
  handle <- openFile filePath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numString
      tasks = lines contents
      bumped = (tasks !! number) : (delete (tasks !! number) tasks)
  hPutStr tempHandle $ unlines bumped
  hClose handle
  hClose tempHandle
  removeFile filePath
  renameFile tempName filePath