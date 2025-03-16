module Tree where

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
