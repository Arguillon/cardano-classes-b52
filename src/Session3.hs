module Session3 where

import           Data.List


--SESSION 3 INTRODUCTION TO HASKELL (13 Jan 2022)
---------------REDEFINE QUICKSORT

-- | Order a list ascending
orderList :: [Int] -> [Int]
orderList []  = []
orderList [x] = [x]
orderList (x : y : xs) | x < y     = x : orderList (y : xs)
                       | otherwise = y : orderList (x : xs)

-- | Iterates on Ordering a list ascending
maximalOrderList :: [Int] -> [Int]
maximalOrderList xs =
    orderList (orderList (orderList (orderList (orderList (orderList xs)))))

-- | Splits a list based on a pivot to keep only smaller values
smallSort :: [Int] -> Int -> [Int]
smallSort [] _ = []
smallSort (x : xs) y | y <= x    = smallSort xs y
                     | otherwise = x : smallSort xs y

-- | Splits a list based on a pivot to keep only bigger values
bigSort :: [Int] -> Int -> [Int]
bigSort [] _ = []
bigSort (x : xs) y | y >= x    = bigSort xs y
                   | otherwise = x : bigSort xs y

-- | Sorts a list based on a pivot
smartSort :: [Int] -> Int -> [Int]
smartSort [] _ = []
smartSort xs y = maximalOrderList (smallSort xs y ++ [y] ++ bigSort xs y)

testValue :: [Int]
testValue = smartSort [9, 6, 7, 2, 3, 4, 1] 4
