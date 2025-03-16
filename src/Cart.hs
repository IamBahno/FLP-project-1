module Cart where
import Data.Map (fromListWith, toList)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.List (partition,elemIndex)
import Data.Maybe (fromJust)


import Tree(TreeNode(EmptyTree))

data DataWithLabel = DataWithLabel String [Float] deriving (Show)
-- it holds the data in list, number of classes, number of samples, and list of pairs (class:nSamples)
data Dataset = Dataset [DataWithLabel] Int Int [(String,Int)] deriving (Show)

-- index of the attreibute and the value
data SplitPoint = SplitPoint Int Float deriving (Show)


parseLabeledDatapoint :: String -> DataWithLabel
parseLabeledDatapoint line =
    let parts = splitOn "," line
        label = last parts
        -- trimm tailing spaces (new line character)
        trimmed_label = reverse $ dropWhile isSpace $ reverse label
        values = map(read :: String -> Float) (init parts)
    in DataWithLabel trimmed_label values 

-- takes lines of file, returns dataset
parseDataset::[String] -> Dataset
parseDataset lines' = createDataset (map parseLabeledDatapoint lines')

-- Smart constructor for Dataset
createDataset :: [DataWithLabel] -> Dataset
createDataset dataList =
    -- get all the labels from the data
    let labels = map (\(DataWithLabel label _) -> label) dataList
        -- maps list of labels into list of (label,1)
        -- use fromListWith to combine the same labels and count it (creates map)
        -- then use toList to apply it
        classCount = toList $ fromListWith (+) $ map (\label -> (label, 1)) labels
        nClasses = length classCount
    in Dataset dataList nClasses (length dataList) classCount

convertToFloats :: [Int] -> [Float]
convertToFloats xs = map fromIntegral xs

giniImpurity :: Dataset -> Float
giniImpurity (Dataset _ _ nSamples classCounts) =
    -- pull out class counts into a list and make them floats
    let floatCounts = convertToFloats $ map snd classCounts
    -- Compute probabilities of classes, raise them to second power and sum them, substract them from one
        giniSum = sum $ map (\x -> (x / fromIntegral nSamples) ** 2) floatCounts
    -- todo, implement some more efficent way
    in 1 - giniSum

-- returns value at given index
getValue :: DataWithLabel -> Int -> Float 
getValue (DataWithLabel _ values) index = values !! index

getNSamples :: Dataset -> Int
getNSamples (Dataset _ _ nSamples _) = nSamples

getData :: Dataset ->  [DataWithLabel]
getData (Dataset listOfLabeledData _ _ _) = listOfLabeledData


-- receives Data and return splitPoints
getSplitPoints :: DataWithLabel -> [SplitPoint]
-- enmurate and zip values into: [(0,y1),(1,y2),(2,y3)...], and then map it to list of SplitPoints
getSplitPoints (DataWithLabel _ values) =map (\(index,value) -> SplitPoint index value) $ zip [0..] values

-- split dataset by split point into two subsets
splitDataSet :: Dataset -> SplitPoint -> (Dataset,Dataset)
splitDataSet (Dataset listOfLabeledData nClasses nSamples classesCounts) (SplitPoint index value) =
    -- partition function splits list into two based on condition, here being the value of attribute at given index
    let (smallerDatasWithLabels,biggerDatasWithLabels) = partition (\x-> (getValue x index) <= value ) listOfLabeledData
    in (createDataset smallerDatasWithLabels, createDataset biggerDatasWithLabels)

-- split dataset by splitpoint and compute gini impurity of the split
evalSplitPoint :: Dataset -> SplitPoint -> Float
evalSplitPoint dataset splitPoint =
    let (subset1,subset2) = splitDataSet dataset splitPoint
        gini1 = giniImpurity subset1
        gini2 = giniImpurity subset2

        -- weight for each gini is nSamples in subset divided the nSamples of the parent dataset
        weight1 = fromIntegral (getNSamples subset1) / fromIntegral (getNSamples dataset)
        weight2 = fromIntegral (getNSamples subset2) / fromIntegral (getNSamples dataset)

    in  weight1 * gini1 +  weight2 * gini2

-- returns index of best split point
findBestSplitPoint :: [SplitPoint] -> Dataset -> Int
findBestSplitPoint splitPoints dataset  =
    --eval the splitpoints
    let splitPointsEvaluations = map (\x -> evalSplitPoint dataset x) splitPoints
        -- fin the smallest value
        minValue = minimum splitPointsEvaluations
    -- return index of the minimum (elemIndex return Maybe, but we are sure that given values is always therem so we can unwrap)
    in fromJust $ elemIndex minValue splitPointsEvaluations

trainTree :: Dataset -> TreeNode
trainTree dataset = 
    -- get lists of split points, and concatenated them
    let possibleSplitPoints = concat $ map getSplitPoints $ getData dataset
        indexOfBestSplit = findBestSplitPoint possibleSplitPoints 
    -- TODO (index should be finished, but is not tested yet)
    in EmptyTree