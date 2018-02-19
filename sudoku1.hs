import Data.Vector as V
import Debug.Trace
import Test.HUnit

type Board = [Int]



--------------------------------
--code for testing board bellow
--------------------------------

--Test board
boardTest :: Board -> Bool
boardTest board = rowTest board && columnTest board && squareTest board

--Test rows
rowTest :: Board -> Bool
rowTest [] = True
rowTest board | test (fst (Prelude.splitAt 9 board)) = rowTest $ snd (Prelude.splitAt 9 board)
              | otherwise = False

--Test columns
columnTest :: Board -> Bool
columnTest board = columnTester 9 board

columnTester :: Int -> Board -> Bool
columnTester n [] = True
columnTester n board | test (column n board) = columnTester (n-1) $ columnDel n board
                     | otherwise = False

columnDel :: Int -> Board -> [Int]
columnDel n [] = []
columnDel n board = (Prelude.take (n-1) (Prelude.drop 1 board)) Prelude.++ columnDel n (Prelude.drop n board)

column :: Int -> Board -> [Int]
column n [] = []
column n board = [Prelude.head board] Prelude.++ column n (Prelude.drop n board)

--Test squares
squareTest :: Board -> Bool
squareTest board = squareTester 9 27 12 board

squareTester :: Int -> Int -> Int -> Board -> Bool
squareTester _ _ _ [] = True
squareTester 0 _ _ board = squareTester 9 27 12 board
squareTester n nn nnn board | test (square n board) = squareTester (n-3) (nn-9) (nnn-6) $ squareDel n nn nnn board
                            | otherwise = False

squareDel :: Int -> Int -> Int -> Board -> [Int]
squareDel n nn nnn [] = []
squareDel n nn nnn board = (Prelude.take (n-3) (Prelude.drop 3 board)) Prelude.++ Prelude.take nnn (squareDel n nn nnn (Prelude.drop n board)) Prelude.++ snd (Prelude.splitAt nn board)

square :: Int -> Board -> [Int]
square n board = (Prelude.take 3 board) Prelude.++ Prelude.take 6 (square n (snd (Prelude.splitAt n board)))

--Test for doublettes in list
test :: Board -> Bool
test (x:[]) = True
test (x:xs) | x == 0 = test xs
            | Prelude.elem x xs = False
            | Prelude.notElem x xs = test xs

------------------------------------------------
--Test cases
------------------------------------------------
test1 = TestCase (assertEqual "for bordTest," True (foo 3))
