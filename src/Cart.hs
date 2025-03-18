{-
    Project: FLP Haskell 2025 - CART decision tree
    login: xbahou00
    name: Ondřej Bahounek
    year: 2025
    description: Implements the CART (Classification and Regression Trees) algorithm.
        By computing Gini impurity for all the data splits at given node and choosing the minimum, while recursively building the tree.
        Also parsing the data on input indo custom data types. 
-}



module Cart where

import Data.Map (fromListWith, toList)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.List (partition,elemIndex)
import Data.Maybe (fromMaybe)

import Tree(TreeNode(Node,Leaf))



-- Data structure representing a labeled data point
data DataWithLabel = DataWithLabel String [Float] deriving (Show)

-- Dataset holds data points in a list, number of classes, number of samples, and list of pairs (class:nSamples)
data Dataset = Dataset [DataWithLabel] Int Int [(String,Int)] deriving (Show)

-- Possible split: index of the attribute and the value
data SplitPoint = SplitPoint Int Float deriving (Show)

-- Parses a single data point from a string
-- Expects N comma seperated floats, and last value is a class label
parseLabeledDatapoint :: String -> DataWithLabel
parseLabeledDatapoint line =
    let parts = splitOn "," line
        label = last parts
        -- trim trailing white spaces (new line)
        trimmedLabel = reverse $ dropWhile isSpace $ reverse label
        values = map read (init parts)
    in DataWithLabel trimmedLabel values 


-- Takes lines of a file, returns dataset
parseDataset::[String] -> Dataset
parseDataset lines' = createDataset (map parseLabeledDatapoint lines')

-- Custom constructor for Dataset
createDataset :: [DataWithLabel] -> Dataset
createDataset dataList =
    -- get all the labels from the data
    let labels = map (\(DataWithLabel label _) -> label) dataList
        -- maps list of labels into list of (label,1)
        -- use fromListWith to combine tuples with the same label and count them (creates map, which is a 'dictionary-like' structure)
        -- then use toList convert it to a list of tuples
        classCount = toList $ fromListWith (+) $ map (\label -> (label, 1)) labels
        nClasses = length classCount
    in Dataset dataList nClasses (length dataList) classCount

convertToFloats :: [Int] -> [Float]
convertToFloats xs = map fromIntegral xs

-- Computes gini impurity of dataset
giniImpurity :: Dataset -> Float
giniImpurity (Dataset _ _ 0 _) = 1 -- if the dataset is empty
giniImpurity (Dataset _ _ _ []) = 1 -- if the dataset is empty
giniImpurity (Dataset _ _ nSamples classCounts) =
    -- pull out class counts into a list and make them floats
    let floatCounts = convertToFloats $ map snd classCounts
    -- Compute probabilities of classes, raise them to second power and sum them
        giniSum = sum $ map (\x -> (x / fromIntegral nSamples) ** 2) floatCounts
    -- substract them from one
    in (1 - giniSum)


-- Retrieves value of attribute at given index from a labeled data point
getValue :: DataWithLabel -> Int -> Float 
getValue (DataWithLabel _ values) index = values !! index

getNSamples :: Dataset -> Int
getNSamples (Dataset _ _ nSamples _) = nSamples

getNClasses :: Dataset -> Int
getNClasses (Dataset _ nClasses _ _) = nClasses

-- Retrieves a class label of a first data point in a dataset
-- Using only when there is only one class left in dataset
getClassFromDataset :: Dataset -> String
getClassFromDataset (Dataset (DataWithLabel className _ : _) _ _ _) = className
getClassFromDataset _ = error "Dataset is empty"

getData :: Dataset ->  [DataWithLabel]
getData (Dataset listOfLabeledData _ _ _) = listOfLabeledData


-- Receives Data and return all possible splitPoints
-- Meaning value in each attribute
getSplitPoints :: DataWithLabel -> [SplitPoint]
-- enmurate and zip values into: [(0,y1),(1,y2),(2,y3)...], and then map it to list of SplitPoints
getSplitPoints (DataWithLabel _ values) =map (\(index,value) -> SplitPoint index value) $ zip [0..] values

-- Split dataset by split point into two subsets
splitDataSet :: Dataset -> SplitPoint -> (Dataset,Dataset)
splitDataSet (Dataset listOfLabeledData _ _ _) (SplitPoint index value) =
    -- partition function splits list into two based on condition, here being the value of attribute at given index
    let (smallerDatasWithLabels,biggerDatasWithLabels) = partition (\x-> (getValue x index) <= value ) listOfLabeledData
    in (createDataset smallerDatasWithLabels, createDataset biggerDatasWithLabels)

-- Split dataset by splitpoint and compute gini impurity of the split
evalSplitPoint :: Dataset -> SplitPoint -> Float
evalSplitPoint dataset splitPoint =
    let (subset1,subset2) = splitDataSet dataset splitPoint
        gini1 = giniImpurity subset1
        gini2 = giniImpurity subset2

        -- weight for each gini is nSamples in subset divided the nSamples of the parent dataset
        weight1 = fromIntegral (getNSamples subset1) / fromIntegral (getNSamples dataset)
        weight2 = fromIntegral (getNSamples subset2) / fromIntegral (getNSamples dataset)

    in  weight1 * gini1 +  weight2 * gini2

-- Finds and returns index of best split point
findBestSplitPoint :: [SplitPoint] -> Dataset -> Int
indBestSplitPoint [] _ = error "Error: No split points given"
findBestSplitPoint splitPoints dataset  =
    --eval the splitpoints
    let splitPointsEvaluations = map (\x -> evalSplitPoint dataset x) splitPoints
        -- find the smallest value
        minValue = minimum splitPointsEvaluations
    -- return index of the minimum (elemIndex return Maybe, so in Nothing case we throw error, since the value has to be there)
    in fromMaybe (error "Error: elemIndex returned Nothing") (elemIndex minValue splitPointsEvaluations)


-- Train a decision tree using the CART algorithm from a dataset
trainTree :: Dataset -> TreeNode
trainTree dataset =
    if getNClasses dataset == 1 -- Only one class left
    then Leaf $ getClassFromDataset dataset
    else
        -- get lists of split points, and concatenated them (flatten the lists)
        let possibleSplitPoints = concat $ map getSplitPoints $ getData dataset
            indexOfBestSplit = findBestSplitPoint possibleSplitPoints dataset
            -- retrieve the best split point
            SplitPoint attributeIndex value = possibleSplitPoints !! indexOfBestSplit 
            -- actually split the dataset by the best splitpoint
            (subset1,subset2) = splitDataSet dataset $ SplitPoint attributeIndex value

        in Node attributeIndex value (trainTree subset1) trainTree subset2

