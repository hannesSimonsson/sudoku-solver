--import Data.Vector as V
import Debug.Trace
import Test.HUnit

type Board = [Int]

data Tree a = Void | T Board [(Tree a)]
     deriving (Show, Eq)

--main
sudoku :: Board -> IO()
sudoku board | length board == 81 && boardTest board && numTest board = boardPrint (stringify (take 81 (traverseTree (buildTree (T board [])))) [])
             | otherwise = putStrLn "Wrong input. A board consists of 81 spaces where a hint is a number 1-9 and empty spaces are represented by 0."

--test that all numbers from a given board is between 0-9
numTest :: Board -> Bool
numTest [] = True
numTest (x:xs) | x <= 0 && x >= 0 = numTest xs
               | otherwise = False


-------------------------------------
--code for returning a solution board
-------------------------------------

--create list of strings from solution
stringify :: Board -> [[Char]] -> [[Char]]
stringify (x:xs) acc = stringify xs (acc ++ [(show x)++"|"])
stringify [] acc = acc

--create string with row of solution
stringRow :: Int -> [[Char]] -> [Char]
stringRow n strBoard = concat $ fst $ splitAt 9 $ snd $ splitAt n strBoard

--print board with solution
boardPrint :: [[Char]] -> IO ()
boardPrint strBoard = do
                   putStrLn "CALCULATING SOLUTION"
                   putStrLn (stringRow 0 strBoard)
                   putStrLn (stringRow 9 strBoard)
                   putStrLn (stringRow 18 strBoard)
                   putStrLn (stringRow 27 strBoard)
                   putStrLn (stringRow 36 strBoard)
                   putStrLn (stringRow 45 strBoard)
                   putStrLn (stringRow 54 strBoard)
                   putStrLn (stringRow 63 strBoard)
                   putStrLn (stringRow 72 strBoard)


-----------------------
--code for bulding tree
-----------------------

--build a tree where all leafs are Void except for the solution
buildTree:: Tree a -> Tree a
buildTree (T [] [(T board1 []),
                 (T board2 []),
                 (T board3 []),
                 (T board4 []),
                 (T board5 []),
                 (T board6 []),
                 (T board7 []),
                 (T board8 []),
                 (T board9 [])]) = T [] [(buildTree (T board1 [])),
                                         (buildTree (T board2 [])),
                                         (buildTree (T board3 [])),
                                         (buildTree (T board4 [])),
                                         (buildTree (T board5 [])),
                                         (buildTree (T board6 [])),
                                         (buildTree (T board7 [])),
                                         (buildTree (T board8 [])),
                                         (buildTree (T board9 []))]
buildTree (T board []) | boardTest board && elem 0 board = buildTree (T [] [(T (changeZero 1 board) []),
                                                                               (T (changeZero 2 board) []), 
                                                                               (T (changeZero 3 board) []), 
                                                                               (T (changeZero 4 board) []), 
                                                                               (T (changeZero 5 board) []), 
                                                                               (T (changeZero 6 board) []), 
                                                                               (T (changeZero 7 board) []), 
                                                                               (T (changeZero 8 board) []), 
                                                                               (T (changeZero 9 board) [])]) 
                       | boardTest board && notElem 0 board = T board []
                       | otherwise = Void

--traverseTree creates a list of possible solutions
traverseTree :: Tree a -> Board
traverseTree (T [] (x:xs)) | x /= Void = [] ++ traverseTree x ++ traverseTree (T [] xs)
                           | x == Void = [] ++ traverseTree (T [] xs)
traverseTree (T board []) | board /= [] = board
                          | board == [] = []


--change first zero to given number
changeZero :: Int -> Board -> Board
changeZero n [] = []
changeZero n (x:xs) | x /= 0 = (x:changeZero n xs)
                    | x == 0 = (n:xs)


------------------------
--code for testing board
------------------------

--Test board
boardTest :: Board -> Bool
boardTest board = rowTest board && columnTest board && squareTest board

--Test rows
rowTest :: Board -> Bool
rowTest [] = True
rowTest board | generalTest (fst (splitAt 9 board)) = rowTest $ snd (splitAt 9 board)
              | otherwise = False

--Test columns
columnTest :: Board -> Bool
columnTest board = columnTester 9 board

columnTester :: Int -> Board -> Bool
columnTester n [] = True
columnTester n board | generalTest (column n board) = columnTester (n-1) $ columnDel n board
                     | otherwise = False

columnDel :: Int -> Board -> [Int]
columnDel n [] = []
columnDel n board = (take (n-1) (drop 1 board)) ++ columnDel n (drop n board)

column :: Int -> Board -> [Int]
column n [] = []
column n board = [head board] ++ column n (drop n board)

--Test squares
squareTest :: Board -> Bool
squareTest board = squareTester 9 27 12 board

squareTester :: Int -> Int -> Int -> Board -> Bool
squareTester _ _ _ [] = True
squareTester 0 _ _ board = squareTester 9 27 12 board
squareTester n nn nnn board | generalTest (square n board) = squareTester (n-3) (nn-9) (nnn-6) $ squareDel n nn nnn board
                            | otherwise = False

squareDel :: Int -> Int -> Int -> Board -> [Int]
squareDel n nn nnn [] = []
squareDel n nn nnn board = (take (n-3) (drop 3 board)) ++ 
                           take nnn (squareDel n nn nnn (drop n board)) ++ 
                           snd (splitAt nn board)

square :: Int -> Board -> [Int]
square n board = (take 3 board) ++ take 6 (square n (snd (splitAt n board)))

--Test for doublettes in list
generalTest :: Board -> Bool
generalTest (x:[]) = True
generalTest (x:xs) | x == 0 = generalTest xs
            | elem x xs = False
            | notElem x xs = generalTest xs

------------------------------------------------
--Test cases
------------------------------------------------
{-test1 = TestCase (assertEqual "for sudoku " [5,3,4,6,7,8,9,1,2,6,7,2,1,9,5,3,4,8,1,9,8,3,4,2,5,6,7,8,5,9,7,6,1,4,2,3,4,2,6,8,5,3,7,9,1,7,1,3,9,2,4,8,5,6,9,6,1,5,3,7,2,8,4,2,8,7,4,1,9,6,3,5,3,4,5,2,8,6,1,7,9] (sudoku [5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8,0,0,0,6,0,0,0,3,4,0,0,8,0,3,0,0,1,7,0,0,0,2,0,0,0,6,0,6,0,0,0,0,2,8,0,0,0,0,4,1,9,0,0,5,0,0,0,0,8,0,0,7,9]))
test2 = TestCase (assertEqual "for sudoku " [6,4,1,7,8,5,2,3,9,7,5,2,9,3,6,8,4,1,8,3,9,1,2,4,7,6,5,9,8,5,3,6,1,4,2,7,3,1,6,2,4,7,9,5,8,4,2,7,5,9,8,3,1,6,2,7,3,6,5,9,1,8,4,5,9,4,8,1,3,6,7,2,1,6,8,4,7,2,5,9,3] (sudoku [0,0,1,7,0,0,0,0,9,0,0,2,9,0,6,8,0,0,0,3,0,0,0,4,0,0,0,0,8,0,3,0,0,0,0,7,0,1,0,0,0,0,0,5,0,4,0,0,0,0,8,0,1,0,0,0,0,6,0,0,0,8,0,0,0,4,8,0,3,6,0,0,1,0,0,0,0,2,5,0,0]))

test3 = TestCase (assertEqual "for generalTest " False (generalTest [0,1,2,3,4,3,2,1,0]))
test4 = TestCase (assertEqual "for generalTest " True (generalTest [0,1,0,2,0,3,0,4,0]))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4]

runtests = runTestTT tests-}
