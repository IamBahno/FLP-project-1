module Main where
import System.Environment (getArgs)
import Data.List.Split (splitOn)

data TreeNode = Node Int Float TreeNode TreeNode 
                | Leaf String 
                |EmptyTree 
                |TmpNode Int Float deriving (Show)


buildTree::[TreeNode] -> (TreeNode,[TreeNode])
buildTree [] = (EmptyTree,[]) -- should not be matched (unles there is empty tree file or something)
buildTree (x:xs) = 
    case x of
        TmpNode attribute value ->
            let (leftChild, remainingNodes) = buildTree xs
                (rightChild, finalRemainingNodes) = buildTree remainingNodes
            in (Node attribute value leftChild rightChild, finalRemainingNodes)
        Leaf className -> (Leaf className,xs)
        _ -> (EmptyTree,[EmptyTree])

parseLine:: String -> TreeNode
parseLine line =
    case words line of -- split the line by words
        ["Leaf:", className] -> Leaf className
        -- using read convert the values into desired datatype (init returns list without the last element, which is "," in this case)
        ["Node:", attribute, value] -> TmpNode (read $ init attribute :: Int) (read value :: Float)
        _ -> EmptyTree -- This should never happen, if the data is valid, which should be by the spicification, so just added case to exhaust options and get rid of warrnings 
constuctTreeFromLines :: [String] -> TreeNode
constuctTreeFromLines fileLines = fst( buildTree (map parseLine fileLines))


readTree :: String -> IO TreeNode
readTree file = do
    wholeFile <- readFile file
    let linesOfFiles = lines wholeFile
    let tree = constuctTreeFromLines linesOfFiles 
    return tree

-- converts strign of comma seperated floats into a list of floats
stringToFloatList :: String -> [Float]
stringToFloatList line = map (read::String -> Float ) (splitOn "," line)


readData :: String -> IO [[Float]]
readData file = do
    wholeFile <- readFile file
    let linesOfFiles = lines wholeFile
    -- list of lists of floats
    let datas = map stringToFloatList linesOfFiles 
    return datas

classifyDataPoint :: TreeNode -> [Float] -> String
classifyDataPoint (Leaf class') _ = class' 
classifyDataPoint (Node index value leftChild rightChild) dataPoint = 
                if (dataPoint !! index) <= value
                then classifyDataPoint leftChild dataPoint 
                else classifyDataPoint rightChild dataPoint
classifyDataPoint _ _ = [] -- should not happen, using just for the compilers ease of mind

classifyDatas :: TreeNode -> [[Float]] -> [String]
classifyDatas tree datas = map (classifyDataPoint tree) datas  

-- TODO test the first part with available tests
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            -- read in tree
            tree <- readTree treeFile
            -- print tree
            dataPoints <- readData dataFile
            -- print dataPoints
            let classifications = classifyDatas tree dataPoints
            -- print classifications
            mapM_ putStrLn classifications
            return ()

            
        ["-2", trainingDataFile] -> do
            return ()

        -- if the parameters are not correct
        _ -> error "Invalid arguments. Usage:\n flp-fun -1 <tree_file> <data_file>\n flp-fun -2 <training_data_file>"