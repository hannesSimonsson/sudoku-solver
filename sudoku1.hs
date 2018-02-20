import Data.Vector as V
import Debug.Trace

type Board = [Int]

data Tree a = Void | T Board [(Tree a)]
     deriving (Show)

sudoku board = buildTree (T board [])

buildTree:: Tree a -> Tree a
--buildTree (T [] [x:xs]) = buildTree (Prelude.concat x)
--buildTree Void = buildTree Void
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

buildTree (T board []) | boardTest board && Prelude.elem 0 board = buildTree (T [] [(T (changeZero 1 board) []), (T (changeZero 2 board) []), (T (changeZero 3 board) []), (T (changeZero 4 board) []), (T (changeZero 5 board) []), (T (changeZero 6 board) []), (T (changeZero 7 board) []), (T (changeZero 8 board) []), (T (changeZero 9 board) [])]) 
                       | boardTest board && Prelude.notElem 0 board = T board []
                       | otherwise = Void



--change first zero to given number
changeZero :: Int -> Board -> Board
changeZero n [] = []
changeZero n (square:board)
    | square /= 0 = (square : changeZero n board)
    | square == 0 = (n : board)

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
