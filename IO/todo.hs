import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main = do
    (command : args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [filename, item] = appendFile filename (item ++ "\n")

view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename
    let tasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [filename, numberstring] = do
    fileHandle <- openFile filename ReadMode
    (tempname, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents fileHandle
    let number = read numberstring
        tasks = lines contents
        newTasks = delete (tasks !! number) tasks
    hPutStr tempHandle $ unlines newTasks
    hClose fileHandle
    hClose tempHandle
    removeFile filename
    renameFile tempname filename



