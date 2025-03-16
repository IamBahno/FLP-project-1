-- DataIO.hs
module DataIO where

import Data.List.Split (splitOn)

-- converts strign of comma seperated floats into a list of floats
stringToFloatList :: String -> [Float]
stringToFloatList line = map (read::String -> Float ) (splitOn "," line)



-- return list of lists of floats, which are datas in each attribute for each data point
readData :: String -> IO [[Float]]
readData file = do
    wholeFile <- readFile file
    let linesOfFiles = lines wholeFile
    -- list of lists of floats
    let datas = map stringToFloatList linesOfFiles 
    return datas

-- Takes in path to a file and returns list of lines
readInFile :: String -> IO [String]
readInFile file = do
    wholeFile <- readFile file -- read in file
    return (lines wholeFile) -- split file into a list line by line
