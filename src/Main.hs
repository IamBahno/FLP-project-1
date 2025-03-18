{-
    Project: FLP Haskell 2025 - CART decision tree
    login: xbahou00
    name: Ondřej Bahounek
    year: 2025
    description: The main file. Parse the command line arguments,
        read in data, parse data, train tree or classify data, print output.
-}

module Main where

import System.Environment (getArgs)


import DataIO(readInFile,readData)
import Tree(constructTreeFromLines)
import Classification(classifyDatas)
import Cart(parseDataset,trainTree)

-- Parse the command line arguments
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> classifyWithTree treeFile dataFile
        ["-2", trainingDataFile] -> trainAndPrintTree trainingDataFile 
        -- if the arguments are not correct
        _ -> error "Invalid arguments. Run with:\n flp-fun -1 <tree_file> <data_file>\n flp-fun -2 <training_data_file>"

-- Function to classify data with given tree
-- First argument is path to a file with tree, second is path to a file with the data.
classifyWithTree :: FilePath -> FilePath -> IO ()
classifyWithTree treeFile dataFile = do
    -- read in tree
    treeLines <- readInFile treeFile
     -- construct tree
    let tree = constructTreeFromLines treeLines
    -- read in data
    dataPoints <- readData dataFile
    -- classify the data
    let classifications = classifyDatas tree dataPoints
    -- print classifications
    mapM_ putStrLn classifications

-- Function to train a decision tree on given dataset and print it
trainAndPrintTree :: FilePath -> IO ()
trainAndPrintTree trainingDataFile = do
    -- read in dataset
    dataLines <- readInFile trainingDataFile
    let dataset = parseDataset dataLines
    let tree = trainTree dataset
    -- print im the correct output format
    print tree