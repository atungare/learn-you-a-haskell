import System.Random
import Control.Monad (when)

main = do
    gen <- getStdGen
    askForNumber gen


askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (rand, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "What number between 1 and 10 am I thinking of?"
    numberStr <- getLine
    when (not $ null numberStr) $ do
        let number = read numberStr :: Int
        if rand == number
            then putStrLn "Wow, you're right!"
            else putStrLn $ "Sorry, I was thinking of " ++ show rand
        askForNumber newGen
