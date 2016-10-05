-- tree2.hs
-- Bob Collins
-- 01-Oct-2016
-- Alternative approach to sorted tree checking plus ordered tree balancing

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Convert tree to a list in node order (left to right)
treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node val left right) =
  (treeToList left ++ [val]) ++ treeToList right 
  
-- Yet another ordered list checker using basic recursion
isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered (x:xs)
   |  null xs = True 
   |  x > head xs = False 
   | otherwise = isOrdered xs

-- Check whether tree is ordered by flattening it first
isTreeOrdered :: (Ord a) => Tree a -> Bool
isTreeOrdered t = isOrdered (treeToList t)

-- Make a balanced tree
makeTree :: [a] -> Tree a
makeTree l  
  | null l = Leaf 
  | length l == 1 = Node (l !! 0) Leaf Leaf 
  | otherwise = let len = (length l)  
                    mid = (round ( fromIntegral len / 2)) in
                    Node (l !! (mid - 1)) (makeTree (take (mid - 1) l)) (makeTree (drop mid l))
               
balanceTree :: Tree a -> Tree a
balanceTree t = makeTree (treeToList t)

-- Add a node to an ordered tree 
addNode :: (Ord a) => a -> Tree a -> Tree a
addNode x Leaf = Node x Leaf Leaf
addNode x (Node val left right)  
   | x > val = Node val left (addNode x right)
   | x < val = Node val (addNode x left) right
   | otherwise = Node val left right
  
