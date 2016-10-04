-- tree.hs
-- Author: Farzad FARID
-- Haskell MOOC week 3

import Data.List

data Tree = Leaf | Node Int Tree Tree deriving Show

-- Sample trees
tsorted = Node 5 (Node 1 Leaf Leaf) (Node 7 Leaf Leaf)
t = (Node 3 (Node 5 (Node 8 Leaf Leaf) Leaf) 
            (Node 7 (Node 2 Leaf Leaf) (Node 9 Leaf Leaf)))

-------------------------------------------------------

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
-- [3,5,8,7,2,9]

-- Convert Tree to list
-- Left subtree, then root, then right subtree 
tree2list' :: Tree -> [Int]
tree2list' Leaf = []
tree2list' (Node x t1 t2) = (tree2list' t1) ++ (x : (tree2list' t2))

-- tree2list' t
-- [8,5,3,2,7,9]

-- Convert Tree to list
-- Left subtree, then right subtree, then root
tree2list'' :: Tree -> [Int]
tree2list'' Leaf = []
tree2list'' (Node x t1 t2) = (tree2list'' t1) ++ (tree2list'' t2) ++ [x]

-- tree2list'' t
-- [8,5,2,9,7,3]

---------------------------------------------------------------------------------

-- Insert Node in order
insertOrdered :: Tree -> Int -> Tree
insertOrdered Leaf n = Node n Leaf Leaf
insertOrdered (Node x t1 t2) n
  | n <= x = Node x (insertOrdered t1 n) t2
  | n > x  = Node x t1 (insertOrdered t2 n)

-- *Main> insertOrdered tsorted 2
-- Node 5 (Node 1 Leaf (Node 2 Leaf Leaf)) (Node 7 Leaf Leaf)

-- *Main> insertOrdered tsorted 6
-- Node 5 (Node 1 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)

-- *Main> insertOrdered tsorted 8
-- Node 5 (Node 1 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf))

-- *Main> insertOrdered tsorted 5
-- Node 5 (Node 1 Leaf (Node 5 Leaf Leaf)) (Node 7 Leaf Leaf)

-- Combination of insertion and conversion to list:
-- *Main> tree2list' $ insertOrdered tsorted 4
-- [1,4,5,7]
-- *Main> tree2list' $ insertOrdered tsorted 5
-- [1,5,5,7]
-- *Main> tree2list' $ insertOrdered tsorted 9
-- [1,5,7,9]
-- *Main> tree2list' $ insertOrdered tsorted 0
-- [0,1,5,7]


--------------------------
-- Balance a tree
-- We first flatten the tree and use the "sort" function from Data.List
balanceTree :: Tree -> Tree
balanceTree = go . sort . tree2list'
    where go :: [Int] -> Tree
          go []     = Leaf
--        go (x:[]) = Node x Leaf Leaf
          go xs     = let
                        mid          = div ((length xs) - 1) 2
                        midValue     = xs !! mid
                        leftSubtree  = take mid xs
                        rightSubtree = drop (mid+1) xs
                      in Node midValue (go leftSubtree) (go rightSubtree)

-- *Main> t
-- Node 3 (Node 5 (Node 8 Leaf Leaf) Leaf) (Node 7 (Node 2 Leaf Leaf) (Node 9 Leaf Leaf))
-- *Main> balanceTree t
-- Node 5 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 8 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf))
