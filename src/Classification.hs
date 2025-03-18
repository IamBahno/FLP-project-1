module Classification where

import Tree(TreeNode(Node,Leaf))


-- clasify one data point
classifyDataPoint :: TreeNode -> [Float] -> String
classifyDataPoint (Leaf class') _ = class' -- if we made it to leaf return the class
classifyDataPoint (Node index value leftChild rightChild) dataPoint = 
                if (dataPoint !! index) <= value -- compera value to data at attribute of given index
                then classifyDataPoint leftChild dataPoint 
                else classifyDataPoint rightChild dataPoint
classifyDataPoint _ _ = [] -- should not happen, using just for the compilers ease of mind

--  classify whole list of data
classifyDatas :: TreeNode -> [[Float]] -> [String]
classifyDatas tree datas = map (classifyDataPoint tree) datas  