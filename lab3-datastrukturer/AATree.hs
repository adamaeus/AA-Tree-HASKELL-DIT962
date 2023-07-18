  {-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  bstHeight,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA trees, represented by two constructors, Empty and Node
-- Node has three arguments, a left subtree, value a, and a right subtree
-- AA tree represented as BST tree
data AATree a
  = Empty
  | Node Int (AATree a) a (AATree a)
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get value (Node _ leftChild nodeValue rightChild)
  | value     == nodeValue = Just nodeValue
  | value     < nodeValue = get value leftChild
  | otherwise = get value rightChild


-- guard to make sure to pattern match only when the insertion created
-- a four-node, that is, when x y and z has the same level
split :: AATree a -> AATree a
split (Node xlvl a x (Node ylvl b y (Node zlvl c z d)))
  |ylvl == xlvl && xlvl == zlvl =  Node (ylvl+1) (Node xlvl a x b) y (Node zlvl c z d)
split tree = tree

-- guard case to ascertain that skew is only applied when a left child
-- is added and has the same as its parent
skew :: AATree a -> AATree a
skew (Node ylvl (Node xlvl a x b) y c)
  | ylvl == xlvl = Node xlvl a x (Node ylvl b y c)
skew tree = tree

-- If value is less than nodeValue, insert to left, and right if larger than
-- After insertion, first skew then split at each binary node up to the root
insert :: Ord a => a -> AATree a -> AATree a
insert value tree = case get value tree of   --case distinction
  Just _  -> tree                            -- if value already exist in tree, return tree
  Nothing -> insertLeaf value tree           -- if it doesn't, insert it
    where
      insertLeaf val Empty = Node 1 Empty val Empty   -- if tree empty, make node with value
      insertLeaf val (Node lvl left nodeValue right)
        | val < nodeValue = split $ skew $ Node lvl (insertLeaf val left) nodeValue right
        | otherwise = split $ skew $ Node lvl left nodeValue (insertLeaf val right)


-- Orders an AA tree and returns a list with the nodes as its elements
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ l root r) = inorder l ++ [root] ++ inorder r

-- calculates the amount of nodes in a tree
size :: AATree a -> Int
size Empty = 0
size (Node _ Empty _ Empty) = 1
size (Node _ l _ r) = size l + size r + 1

-- returns the level (height) when seen as an AA tree
height :: AATree a -> Int
height Empty = 0
height (Node lvl _ _ _) = lvl


-- Function to calculate height seen as a binary tree
-- Exposed to main via the module
bstHeight :: AATree a -> Int
bstHeight Empty = 0
bstHeight (Node _ l _ r) = 1 + max (bstHeight l) (bstHeight r)


--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants
checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered in ascending order
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs)
  | x <= y    = isSorted (y:xs)
  | otherwise = False


-- checks that the left childs height is less than its parent
leftChildOK :: Ord a => AATree a -> Bool
leftChildOK Empty = True
leftChildOK n = heightDiff == 1
  where
    heightDiff = height n - height (leftSub n)

-- checks that the right childs height is less than or equal to its parent
rightChildOK :: Ord a => AATree a -> Bool
rightChildOK Empty = True
rightChildOK n = heightDiff == 0 || heightDiff == 1
  where
    heightDiff = height n - height (rightSub n)

-- checks that the rightgrandchilds height is less than that of its parent
rightGrandChildOK :: Ord a => AATree a -> Bool
rightGrandChildOK Empty = True
rightGrandChildOK n = heightDiff == 1 && heightDiff' == 0 || heightDiff' == 1
  where
    heightDiff' = height n - height (rightSub n)
    heightDiff = height n - height (rightSub (rightSub n))

-- Makes sure that the level invariant hold for AA trees
checkLevels :: Ord a => AATree a -> Bool
checkLevels node = leftChildOK node && rightChildOK node && rightGrandChildOK node

--chicks if AA tree is empty
isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- returns node left of its parents node
leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node _ left _ _) = left

--returns node right of its parent node
rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ _ _ right) = right


--------------------------------------------------------------------------------
