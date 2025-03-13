module Main where
import System.Environment (getArgs)
import Data.List.Split (splitOn)

--  data type for storing the tree
data TreeNode = Node Int Float TreeNode TreeNode 
                | Leaf String 
                |EmptyTree 
                |TmpNode Int Float   -- repres node which doest have assigned children yet, used while parsing the input
                deriving (Show)

-- takes list of Leafs and TmpNodes and returns tree and unused rest of list
buildTree::[TreeNode] -> (TreeNode,[TreeNode])
buildTree [] = (EmptyTree,[]) -- should not be matched (unles there is empty tree file or something)
buildTree (x:xs) = 
    case x of
        TmpNode attribute value ->
            -- first build left child, then take the returned list, and build the right child
            let (leftChild, remainingNodes) = buildTree xs
                (rightChild, finalRemainingNodes) = buildTree remainingNodes
            in (Node attribute value leftChild rightChild, finalRemainingNodes)
        Leaf className -> (Leaf className,xs)
        _ -> (EmptyTree,[EmptyTree])



-- line of input int a Node
parseLine:: String -> TreeNode
parseLine line =
    case words line of -- split the line by words
        ["Leaf:", className] -> Leaf className
        -- using read convert the values into desired datatype (init returns list without the last element, which is "," in this case)
        ["Node:", attribute, value] -> TmpNode (read $ init attribute :: Int) (read value :: Float)
        _ -> EmptyTree -- This should never happen, if the data is valid, which should be by the spicification, so just added case to exhaust options and get rid of warrnings 

-- Takes inlist of lines of tree returns tree
constuctTreeFromLines :: [String] -> TreeNode
-- parse lines into nodes, build tree and take only first value of of the returned tuple, which is tree
constuctTreeFromLines fileLines = fst( buildTree (map parseLine fileLines))

-- Takes in path to a file and returns a tree
readTree :: String -> IO TreeNode
readTree file = do
    wholeFile <- readFile file -- read in file
    let linesOfFiles = lines wholeFile -- split file into a list line by line
    let tree = constuctTreeFromLines linesOfFiles -- construct tree
    return tree

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

-- clasify one data point
classifyDataPoint :: TreeNode -> [Float] -> String
classifyDataPoint (Leaf class') _ = class' -- if we made it to leaf return the class
classifyDataPoint (Node index value leftChild rightChild) dataPoint = 
                if (dataPoint !! index) <= value -- compera value to data at attribute of given index
                then classifyDataPoint leftChild dataPoint 
                else classifyDataPoint rightChild dataPoint
classifyDataPoint _ _ = [] -- should not happen, using just for the compilers ease of mind

- classify whole list of data
classifyDatas :: TreeNode -> [[Float]] -> [String]
classifyDatas tree datas = map (classifyDataPoint tree) datas  

-- main
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-1", treeFile, dataFile] -> do
            -- read in tree
            tree <- readTree treeFile
            -- read in data
            dataPoints <- readData dataFile
            -- classify the data
            let classifications = classifyDatas tree dataPoints
            -- print classifications
            mapM_ putStrLn classifications
            return ()

            
        ["-2", trainingDataFile] -> do
            return ()

        -- if the parameters are not correct
        _ -> error "Invalid arguments. Run with:\n flp-fun -1 <tree_file> <data_file>\n flp-fun -2 <training_data_file>"