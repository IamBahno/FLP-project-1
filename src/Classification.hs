{-
    Project: FLP Haskell 2025 - CART decision tree
    login: xbahou00
    name: Ondřej Bahounek
    year: 2025
    description: Classifying data using built tree.
-}

module Classification where

import Tree(TreeNode(Node,Leaf))


-- Classify a single data point based on the decision tree
classifyDataPoint :: TreeNode -> [Float] -> String
classifyDataPoint (Leaf classLabel) _ = classLabel  -- If we reach a leaf, return the class label

classifyDataPoint (Node index value leftChild rightChild) dataPoint = 
                if (dataPoint !! index) <= value -- Compare node value to datapoint at given index
                then classifyDataPoint leftChild dataPoint -- Traverse left if value is smaller/equal
                else classifyDataPoint rightChild dataPoint -- Traverse right otherwise
classifyDataPoint _ _ = error "Erro: Cannot classify, either incorrect tree or data point." -- Shouldn't happen

--  Classify whole list of data
classifyDatas :: TreeNode -> [[Float]] -> [String]
classifyDatas tree datas = map (classifyDataPoint tree) datas
