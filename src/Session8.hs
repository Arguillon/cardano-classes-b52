module Session8 where

import           Data.List


    --SESSION 8 RECURSIVE PATTERNS  (06 Feb 2022)

{-
-- EXERCICE 1 SKIP FUNCTION
Output is a list of lists
The first list is the same as the input list
The second list should contain every second element
The nth list should contain every nth element

Example:

skips "ABCD"       == ["ABCD", "BD", "C", "D"]
skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1]          == [[1]]
skips [True,False] == [[True,False], [False]]
skips []           == []
-}


-- | Turns a list into a tupple of (index, element)
turnListIntoTuple :: [a] -> [(Int, a)]
turnListIntoTuple []       = []
turnListIntoTuple (x : xs) = zip [1 .. (length (x : xs))] (x : xs)

-- | Gets every nth element (multiple of n) in a list based on the index of the element
getEveryNthElement :: Int -> [(Int, a)] -> [a]
getEveryNthElement _ [] = []
getEveryNthElement 0 _  = []
getEveryNthElement y ((index, value) : xs)
    | index `mod` y == 0 = value : getEveryNthElement y xs
    | otherwise          = getEveryNthElement y xs


-- | Returns all the elements multiple of every Integers inside a list passed in parameter
-- | returnEveryNthElement [1,2,3] "Hello" = "Hello" : "el" : "l" 
returnEveryNthElement :: [Int] -> [a] -> [[a]]
returnEveryNthElement _  [] = []
returnEveryNthElement [] _  = []
returnEveryNthElement (index : indexes) elements =
    getEveryNthElement index (turnListIntoTuple elements)
        : returnEveryNthElement indexes elements

-- | Function that returns a list of list composed of
-- | 1 The whole content of the list     
-- | 2 Every element of the list which index is multiple of 2     
-- | 2 Every element of the list which index is multiple of 3     
-- | 2 Every element of the list which index is multiple of 4     
skips :: [a] -> [[a]]
skips [] = []
skips x  = returnEveryNthElement [1 .. (length x)] x





{- EXERCICE 2 -- LOCAL MAXIMA
A LOCAL MAXIMUM OF A LIST IS AN ELEMENT OF THE LIST WHICH IS STRICTLY GREATER THAN BOTH 
[1,2,3,4,5] -> []
[1,2,3,4,2,5,3] -> [4,5]
-}

-- | Gets the local maxima of a list of Integers
-- | A local maxima is defined as the highest element of a subset of a list
-- | getLocalMaxima [1,2,3,4,2,5,3] = [4,5]
getLocalMaxima :: [Int] -> [Int]
getLocalMaxima []     = []
getLocalMaxima [_, _] = []
getLocalMaxima [_]    = []
getLocalMaxima (x : y : z : xs) | y > x && y > z = y : getLocalMaxima (z : xs)
                                | otherwise      = getLocalMaxima (y : z : xs)

{- EXERCICE 3 HISTOGRAM
-}


-- | Returns the number of times an element appears in a list into little star characters
-- | If the number of occurences is equal to 0 then it returns a space
-- | countOccurenceOfElementInList 2 [1,2,3,5,2,4,2] = ***
-- SOLUTION ? METTRE DES ESPACES JUSQUA ARRIVER A LINDEX ? 
countOccurenceOfElementInList :: Int -> [Int] -> String
countOccurenceOfElementInList index x
    | not (null (filter (== index) x)) = replicate
        (length (filter (== index) x))
        '*'
    | otherwise = " "

-- | Returns the number of times every element of a list appears in that list
countOccurencesOfEveryElementInList :: [Int] -> [Int] -> [String]
countOccurencesOfEveryElementInList _  [] = []
countOccurencesOfEveryElementInList [] _  = []
countOccurencesOfEveryElementInList (index : indexes) elements =
    countOccurenceOfElementInList index elements
        : countOccurencesOfEveryElementInList indexes elements

-- | Base for counting elements
listOfeveryInteger :: [Int]
listOfeveryInteger = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]


-- | Base for histogram
histogramBase :: [Char]
histogramBase = intercalate "\n" ["===================", "0,1,2,3,4,5,6,7,8,9"]

-- | Turns the list of occurence of integer into a tupple (number,occurence)
turnListIntoTupleWithOccurence :: [Int] -> [(Int, String)]
turnListIntoTupleWithOccurence x = zip
    listOfeveryInteger
    (countOccurencesOfEveryElementInList listOfeveryInteger x)

-- | Shaping of every tuple into a string for the histogram
-- | [(1,*),(2,**)] = 1|*\n2|**
getEveryValueFromListOfTuple :: [(Int, String)] -> [String]
getEveryValueFromListOfTuple [] = []
getEveryValueFromListOfTuple ((x, y) : xs) =
    (show x ++ "|" ++ y ++ "\n") : getEveryValueFromListOfTuple xs

-- | Returns an histogram from a list of Int
-- | note : This histogram is y-axis oriented. the exercise asked for an x-axis oriented
getHistogram :: [Int] -> String
getHistogram [] = []
getHistogram x =
    unlines (getEveryValueFromListOfTuple (turnListIntoTupleWithOccurence x))


-- | Used for testing
testList :: [Int]
testList = [1, 2, 3, 4, 6, 9, 7, 5, 3, 4, 2, 3, 1]


--- SECOND WAY OF THINKING --> WITH GRIDS AND COORDONATES ( a bit overkill ?)
grid :: Int -> Int -> [(Int, Int, Char)]
grid m n = [ (x, y, z) | x <- [0 .. m], y <- [0 .. n], z <- "_" ]


countOccurenceOfElementInListGrid :: Int -> [Int] -> Int
countOccurenceOfElementInListGrid index x = length (filter (== index) x)

-- | Returns the number of times every element of a list appears in that list
countOccurencesOfEveryElementInListGrid :: [Int] -> [Int] -> [Int]
countOccurencesOfEveryElementInListGrid _  [] = []
countOccurencesOfEveryElementInListGrid [] _  = []
countOccurencesOfEveryElementInListGrid (index : indexes) elements =
    countOccurenceOfElementInListGrid index elements
        : countOccurencesOfEveryElementInListGrid indexes elements

-- | Turns the list of occurence of integer into a tupple (number,occurence)
turnListIntoTupleWithOccurenceGrid :: [Int] -> [(Int, Int)]
turnListIntoTupleWithOccurenceGrid x = zip
    listOfeveryInteger
    (countOccurencesOfEveryElementInListGrid listOfeveryInteger x)

-- | If there is one occurence of a number (value inside tupple > 0) then one * is added
-- | putOccurenceInGrid (grid 2 2)  [(1,1)] = [(0,0,'_'),(0,1,'_'),(0,2,'_'),(1,0,'*'),(1,1,'_'),(1,2,'_'),(2,0,'_'),(2,1,'_'),(2,2,'_')]
putOccurenceInGrid :: [(Int, Int, Char)] -> [(Int, Int)] -> [(Int, Int, Char)]
putOccurenceInGrid [] _  = []
putOccurenceInGrid x  [] = x
putOccurenceInGrid ((x, y, z) : xs) ((index, value) : values)
    | value == 0 = (x, y, z) : putOccurenceInGrid xs values
    | index == x = (x, y, '*')
    : putOccurenceInGrid xs ((index, value - 1) : values)
    | otherwise = (x, y, z) : putOccurenceInGrid xs ((index, value) : values)


-- | Turns a grid into a String, when the last value of a index (x,Y,z)is reached then a \n is added
turnGridIntoString :: [(Int, Int, Char)] -> [String]
turnGridIntoString [] = []
turnGridIntoString ((a, b, c) : xs)
    | b == 9    = (c : "\n") : turnGridIntoString xs
    | otherwise = [c] : turnGridIntoString xs

-- | Another way of getting an histogram this time working with grids
-- turnListIntoTupleWithOccurenceGrid returns a list of tuples with the number of times an int appears
-- putOccurenceInGrid is used to populate the grid
-- reverse is used to mirror the result (asked to correspond to the result of the exercice)
-- concat merges every sublist
-- lines creates sublists based on the '\n' character
-- reverse is used to correspond to the result of the exercice
-- intercalate "\n" is used to add a return to the line after every
getHistogramInsaneMethod :: [(Int, Int, Char)] -> [Int] -> String
getHistogramInsaneMethod x y =
    intercalate
            "\n"
            (transpose
                (lines
                    (concat
                        (reverse
                            (turnGridIntoString
                                (putOccurenceInGrid
                                    x
                                    (turnListIntoTupleWithOccurenceGrid y)
                                )
                            )
                        )
                    )
                )
            )
        ++ "\n9876543210"
