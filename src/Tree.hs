{-
    Project: FLP Haskell 2025 - CART decision tree
    login: xbahou00
    name: Ondřej Bahounek
    year: 2025
    description: Define TreeNode datatype and functions for parsing input and building tree.
-}


module Tree where

--  data type for storing the tree
data TreeNode = Node Int Float TreeNode TreeNode 
                |Leaf String 
                |EmptyTree 
                |TmpNode Int Float   -- represent node which doest have assigned children yet, used while parsing the input

-- custom show for TreeNode
instance Show TreeNode where
    show tree = showTree tree 0

-- Takes in tree adn depth on a node in a tree
-- returns string of tree in format in which it should be printed
showTree :: TreeNode -> Int -> String
showTree (Node index value leftChild rightChild) depth = 
    replicate (depth * 2) ' ' ++ "Node: " ++ show index ++ ", " ++ show value ++ "\n"
    ++ showTree leftChild (depth + 1)
    ++ showTree rightChild (depth + 1)
showTree (Leaf label) depth  = replicate (depth * 2) ' ' ++ "Leaf: " ++ label ++ "\n"
showTree EmptyTree depth = replicate (depth * 2) ' ' ++ "EmptyTree\n"
showTree (TmpNode index value) depth = replicate (depth * 2) ' ' ++ "TmpNode: " ++ show index ++ ", " ++ show value ++ "\n" 



-- takes list of Leafs and TmpNodes and returns tree and unused rest of list
buildTree::[TreeNode] -> (TreeNode,[TreeNode])
buildTree [] = error "Cannot build tree with no nodes." -- should not be matched (unles there is empty tree file or something)
buildTree (x:xs) = 
    case x of
        TmpNode attribute value ->
            -- first build left child, then take the returned list, and build the right child
            let (leftChild, remainingNodes) = buildTree xs
                (rightChild, finalRemainingNodes) = buildTree remainingNodes
            in (Node attribute value leftChild rightChild, finalRemainingNodes)
        Leaf className -> (Leaf className,xs)
        _ -> error "Unexpected node type encountered during tree construction (or just any unexpected datatype)."

-- line of input int a Node
parseLine:: String -> TreeNode
parseLine line =
    case words line of -- split the line by words
        ["Leaf:", className] -> Leaf className
        -- using read convert the values into desired datatype (init returns list without the last element, which is "," in this case)
        ["Node:", attribute, value] -> TmpNode (read $ init attribute :: Int) (read value :: Float)
        _ -> error "Input tree is in incorrect format." -- This should never happen, if the data is valid, which should be by the specification, so just added case to exhaust options and get rid of warnings 

-- Takes inlist of lines of tree returns tree
constructTreeFromLines :: [String] -> TreeNode
-- parse lines into nodes, build tree and take only first value of of the returned tuple, which is tree
constructTreeFromLines fileLines =
    let (tree,_) = buildTree (map parseLine fileLines)
    in tree