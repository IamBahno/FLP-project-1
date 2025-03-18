{-
    Project: FLP Haskell 2025 - CART decision tree
    login: xbahou00
    name: Ondřej Bahounek
    year: 2025
    description: Functions for reading data from a file.
-}


module DataIO where

import Data.List.Split (splitOn)

-- Converts strings of comma seperated floats into a list of floats
stringToFloatList :: String -> [Float]
stringToFloatList line = map (read::String -> Float ) (splitOn "," line)



-- Return list of lists of floats, which are datas in each attribute for each data point
readData :: String -> IO [[Float]]
readData file = do
    wholeFile <- readFile file
    let linesOfFiles = lines wholeFile
    -- List of lists of floats
    let datas = map stringToFloatList linesOfFiles 
    return datas

-- Takes in path to a file and returns list of lines
readInFile :: String -> IO [String]
readInFile file = do
    wholeFile <- readFile file -- read in file
    return (lines wholeFile) -- split file into a list line by line
