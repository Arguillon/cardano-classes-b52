module Session5 where

import           Data.List


--SESSION 5 DATA TYPES  (23 jan 2022)
--------------------------------------------------------------
------EXERCICE JEDI LOGS--------------------------------------
--------------------------------------------------------------

--------------------------------------------------------------
--- 1 --- LOG FILE PARSING
--------------------------------------------------------------

data LogMessage
    = InformationLog Int String
    | WarningLog Int String
    | ErrorLog Int Int String
    | UnknownLog String
    deriving (Show,Eq)

-- | The logs R2D2 was able to give us
jediLogToOrder :: String
jediLogToOrder =
    "E 1 1 There seems to be an error with C3PO\n I 2 Kit Fisto has arrived in Coruscant\n W 3 Rogue Squadron blue seems to be on low fuel, please go to the nearest gas station \n I 4 Bacta is on sale on Kashyyk\n E 50 5 Energy shield of A-Wing 5024625 is not working anymore\n W 8 Ionic bomb is coming near, setting Energy shield to maximum power \nE 90 9 A-Wing Blue 5024625 is not responding anymore\n X This is an unknown log\n"

-- | Separates every words in a String 
getWordsFromLine :: [String] -> [[String]]
getWordsFromLine [] = []
getWordsFromLine (x : xs) | null xs   = [words x]
                          | otherwise = words x : getWordsFromLine xs

-- | For each log in a list of logs, returns a LogMessage
createLogMessageFromList :: [[String]] -> [LogMessage]
createLogMessageFromList [[]] = [UnknownLog []]
createLogMessageFromList []   = [UnknownLog []]
createLogMessageFromList (x : xs)
    | null xs   = [createLogMessageFromString x]
    | otherwise = createLogMessageFromString x : createLogMessageFromList xs



-- | Checks the first argument in a list of String in order to create the appropriate Message Type
createLogMessageFromString :: [String] -> LogMessage
createLogMessageFromString [] = UnknownLog []
createLogMessageFromString (x : y : z : xs)
    | x == ['E'] = ErrorLog (read y :: Int) (read z :: Int) (unwords xs)
    | x == ['I'] = InformationLog (read y :: Int) (unwords (z : xs))
    | x == ['W'] = WarningLog (read y :: Int) (unwords (z : xs))
    | otherwise  = UnknownLog (unwords (x : y : z : xs))

-- | Takes a String of unorganized logs, separated by a \n and returns a list of LogMessages
parseLog :: String -> [LogMessage]
parseLog x = createLogMessageFromList (getWordsFromLine (lines x))

--------------------------------------------------------------
--- 2 --- PUTTING THE LOGS IN ORDER
--------------------------------------------------------------

--- a INSERT INTO TREE


data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
                 deriving (Show,Eq)

-- | Gets the timestamp from a LogMessage based on its type
getTimeStampFromLog :: LogMessage -> Int
getTimeStampFromLog (InformationLog x y) = x
getTimeStampFromLog (WarningLog     x y) = x
getTimeStampFromLog (ErrorLog x y z    ) = y


logError :: LogMessage
logError = ErrorLog 1 58 "There seems to be an error with C3PO"

logInfo :: LogMessage
logInfo = InformationLog 3 "Kit Fisto has arrived in Coruscant"

logWarning :: LogMessage
logWarning = WarningLog 42 "Warning Warning"

logToAdd :: LogMessage
logToAdd = InformationLog 60 "Hello there"

logNotAdded :: LogMessage
logNotAdded = UnknownLog "This is an error log"

logTree :: MessageTree
logTree = Node (Node Leaf logWarning Leaf) logInfo (Node Leaf logError Leaf)

insertLog :: LogMessage -> MessageTree -> MessageTree
insertLog (UnknownLog u) x    = x
insertLog log            Leaf = Node Leaf log Leaf
insertLog log (Node left logNode right)
    | getTimeStampFromLog log > getTimeStampFromLog logNode = Node
        left
        logNode
        (insertLog log right)
    | getTimeStampFromLog log < getTimeStampFromLog logNode = Node
        (insertLog log left)
        logNode
        right
    | getTimeStampFromLog log == getTimeStampFromLog logNode = Node left
                                                                    logNode
                                                                    right

--- B ORDER BINARY TREE

-- | Gets the timestamp from a MessageTree
getTimeStampFromTree :: MessageTree -> Int
getTimeStampFromTree Leaf                      = 0
getTimeStampFromTree (Node left logNode right) = getTimeStampFromLog logNode

-- | Sorts a list of logs based on their timestamp ascending
inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left logNode right)
    | getTimeStampFromLog logNode < getTimeStampFromTree left
    = [logNode] ++ inorder left ++ inorder right
    | otherwise
    = inorder left ++ [logNode] ++ inorder right

---c WHAT WENT WRONG 

-- | Sorts a list of logs based on their timestamp ascending
sortLogs :: [LogMessage] -> [LogMessage]
sortLogs []  = []
sortLogs [x] = [x]
sortLogs (x : y : xs)
    | getTimeStampFromLog x < getTimeStampFromLog y = x : sortLogs (y : xs)
    | otherwise = y : sortLogs (x : xs)

-- | in order to be sure to have a list ascending, I need to repeat the sorting algorithm a lot of times, overkill solution but the only one I can think of right now
repeteSortLogs :: [LogMessage] -> [LogMessage]
repeteSortLogs x =
    sortLogs (sortLogs (sortLogs (sortLogs (sortLogs (sortLogs x)))))


-- | Just  to have it quick and easy under the hand
listOfLogs :: [LogMessage]
listOfLogs =
    [ ErrorLog 1 58 "There seems to be an error with C3PO"
    , InformationLog 3 "Kit Fisto has arrived in Coruscant"
    , WarningLog
        1
        "Rogue Squadron blue seems to be on low fuel, please go to the nearest gas station"
    , InformationLog 4 "Bacta is on sale on Kashyyk"
    , ErrorLog 50 6 "Energy shield of A-Wing 5024625 is not working anymore"
    , WarningLog
        8
        "Ionic bomb is coming near, setting Energy shield to maximum power"
    , ErrorLog 90 1 "A-Wing Blue 5024625 is not responding anymore"
    , UnknownLog "This is an error Log"
    ]


-- | Takes a list of unordered logs and returns only the error message with error log of 50 and more
whatWentWrong :: [LogMessage] -> [LogMessage]
whatWentWrong [] = []
whatWentWrong ((ErrorLog x y z) : xs) | x >= 50 =
    repeteSortLogs (ErrorLog x y z : whatWentWrong xs)
whatWentWrong (x : xs) = whatWentWrong xs
