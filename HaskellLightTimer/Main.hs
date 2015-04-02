module Main where
import TrafficReading
import Traffic
import System.IO  
import System.Environment
import Control.Monad
import System.Console.GetOpt
import Timings
import RandomM
import System.Random

data Flag = Time | Number Int | Help deriving (Eq, Ord, Show, Read)

options = [Option ['t'] ["time"] (NoArg (Time)) "Output how long it will take to clear the grid.",
           Option ['n'] ["num", "number"] (ReqArg (\s -> Number $ read s) "number") 
                  "Search up to 'num' possible timings.",
           Option ['h'] ["help"] (NoArg (Help)) "print out a help message and quit the program."]

iterationsOfFlags :: [Flag] -> Int
iterationsOfFlags [] = 5000
iterationsOfFlags (Number k:flags) = k
iterationsOfFlags (_:flags) = iterationsOfFlags flags

main :: IO ()
main = do 
    args <- getArgs
    let (flags, fileargs, errors) = getOpt Permute options args
    if length fileargs == 0
    then putStrLn $ usageInfo "Traffic <file name>" options 
    else if Help `elem` flags
    then putStrLn "Use a txt file you wrote and call me with that as a parameter."
    else do 
        handle <- openFile (fileargs!!0) ReadMode
        contents <- hGetContents handle
        let (inters, cars) = parseFile contents
        let num = iterationsOfFlags flags
        gen <- getStdGen
        let timings = evalRand (randNTimings num inters (5,180)) gen
        let answer = findBestTiming timings cars num
        let output = prettyPrint (Time `elem` flags) answer
        putStr output
        return ()
