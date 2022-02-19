module Session10 where

import           Data.List


    --SESSION 10 HIGHER ORDER FUNCTIONS (12 Feb 2022)
func1 :: [Int] -> Int
func1 [] = 0
func1 (x : xs) | even x    = (x - 2) * func1 xs
               | otherwise = func1 xs


func1' :: [Int] -> Int
func1' = foldr (*) 1 . map (\x -> x - 2) . filter even

func2 :: Int -> Int
func2 1 = 0
func2 n | even n    = n + func2 (n `div` 2)
        | otherwise = func2 (3 * n + 1)


func2' :: Int -> Int
func2' = sum . filter even . takeWhile (> 1) . iterate
    (\x -> case () of
        _ | even x -> x `div` 2
          | odd x  -> 3 * x + 1
    )

-- | Returns true of there is an odd number of True in a list of Bool, False instead
nXor :: [Bool] -> Bool
nXor = odd . length . filter (== True)

-- | Returns list of prime numbers until n
sieveSundaram :: Int -> [Int]
sieveSundaram n = filter
    (<= n)
    (map
        (\x -> 2 * x + 1)
        ([1 .. n]
   -- \\ operator is used to keep elements that are not present in two lists
                  \\ filter
            (<= n)
            [ i + j + 2 * i * j | i <- [1 .. n], j <- [i .. n] ]
        )
    )

data Tree a = Leaf | Node Int (Tree a) a (Tree a) deriving (Show,Eq)


--unused
createTuppleFromTree :: Tree a -> (Int, Int)
createTuppleFromTree Leaf = (0, 0)
createTuppleFromTree (Node _ leftSubtree _ rightSubtree) =
    (treeDepth leftSubtree, treeDepth rightSubtree)
--unused
howLongIsAtree :: [a] -> Int
howLongIsAtree = treeDepth . foldTree

-- | Returns the length of a tree from top to bottom
treeDepth :: Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree _ rightSubtree) =
    1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

-- | Fold a list of A into a binary tree
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node longTree (foldTree front) x (foldTree back)  where
    longTree          = treeDepth (Node n (foldTree front) x (foldTree back))
    n                 = length xs
    (front, x : back) = splitAt (n `div` 2) xs

-- | returns true if a tree is balanced
-- a tree is balanced if the length of its right side is equal (+- 1) to the left length of its left side
isBalancedTree :: Tree a -> Bool
isBalancedTree Leaf = False
isBalancedTree (Node _ leftSubtree _ rightSubtree) =
    substraction == 1 || substraction == 0 || substraction == 1
    where substraction = treeDepth leftSubtree - treeDepth rightSubtree


-- balancedTree
balancedTree :: Tree Char
balancedTree = foldTree "ABCDEFWXCVBNQSDFGHJKMPLOHDFRHBF"

-- unbalanced tree
unbalancedTree :: Tree Char
unbalancedTree = Node
    5
    (Node
        4
        (Node 3
              (Node 2 (Node 1 Leaf 'A' Leaf) 'B' (Node 1 Leaf 'C' Leaf))
              'D'
              (Node 2 (Node 1 Leaf 'E' Leaf) 'F' (Node 1 Leaf 'W' Leaf))
        )
        'X'
        (Node 3
              (Node 2 (Node 1 Leaf 'C' Leaf) 'V' (Node 1 Leaf 'B' Leaf))
              'N'
              (Node 2 (Node 1 Leaf 'Q' Leaf) 'S' (Node 1 Leaf 'D' Leaf))
        )
    )
    'F'
    (Leaf)
