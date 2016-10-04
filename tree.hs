-- tree.hs
-- Author: Farzad FARID
-- Haskell MOOC week 3

data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
    1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

-- treeDepth (Node 3 Leaf (Node 42 (Node 5 Leaf Leaf) Leaf))

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node n leftSubtree rightSubtree) =
    n + (treeSum leftSubtree) + (treeSum rightSubtree)

-- treeSum (Node 3 Leaf (Node 42 (Node 5 Leaf Leaf) Leaf))

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x < maxVal && leftSorted && rightSorted

-- isSortedTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) minBound maxBound

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
addNewMax (Node x t1 t2)   = Node x t1 (addNewMax t2)

-- addNewMax (Node 3 Leaf (Node 42 (Node 5 Leaf Leaf) Leaf))

-------------------------------------------------------------------------------

-- Convert Tree to list
-- Root first, then left subtree, then right subtree 
tree2list :: Tree -> [Int]
tree2list Leaf = []
tree2list (Node x t1 t2) = x : ((tree2list t1) ++ (tree2list t2))

-- tree2list t

-- Convert Tree to list
-- Left subtree, then root, then right subtree 
tree2list' :: Tree -> [Int]
tree2list' Leaf = []
tree2list' (Node x t1 t2) = (tree2list' t1) ++ (x : (tree2list' t2))

-- tree2list' t

-- Convert Tree to list
-- Left subtree, then right subtree, then root
tree2list'' :: Tree -> [Int]
tree2list'' Leaf = []
tree2list'' (Node x t1 t2) = (tree2list'' t1) ++ (tree2list'' t2) ++ [x]

-- tree2list'' t

t = (Node 3 (Node 5 (Node 8 Leaf Leaf) Leaf) 
            (Node 7 (Node 2 Leaf Leaf) (Node 9 Leaf Leaf)))

