module Main where

import Lib
import System.IO

mappingFromFile :: String -> IO (String -> [String])
mappingFromFile fileName =  do
    contents <- readFile fileName
    return (buildMapping $ lines contents)

readAndPrint :: (String -> [String]) -> IO ()
readAndPrint mapping = do
    putStr "Word: "
    hFlush stdout
    word <- getLine
    let anagram = take 1 (anagramN mapping word (length word))
    case anagram of
        [] -> putStrLn "No results found."
        _ -> putStrLn (head anagram)
    readAndPrint mapping

main :: IO ()
main = do
    putStr "Path to words list: "
    hFlush stdout
    filePath <- getLine
    mapping <- mappingFromFile filePath
    readAndPrint mapping