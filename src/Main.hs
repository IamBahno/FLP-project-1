module Main where
import System.Environment (getArgs)
import DataIO
import Tree
import Classification


-- main
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            -- read in tree
            treeLines <- readInTree treeFile
            let tree = constuctTreeFromLines treeLines -- construct tree

            -- read in data
            dataPoints <- readData dataFile
            -- classify the data
            let classifications = classifyDatas tree dataPoints
            -- print classifications
            mapM_ putStrLn classifications
            return ()

            
        ["-2", trainingDataFile] -> do
            -- read in data
            -- dataPoints <- readDataset trainingDataFile
            return ()

        -- if the parameters are not correct
        _ -> error "Invalid arguments. Run with:\n flp-fun -1 <tree_file> <data_file>\n flp-fun -2 <training_data_file>"