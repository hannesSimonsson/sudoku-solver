--example board
--[[0,1,0],
-- [1,2,3],
-- [2,0,0]]
type Board = [[Int]]

sudoku :: Board -> Board
sudoku board
    | notElem 0 (concat Board) = board
    | elem 0 (concat Board) = solve board

solve board = sudoku $ solveCoorndinate board (findZero board 1)

solveCoordinate :: Board -> (Int,Int) -> Board


-- search takes the board and a coordinate and gives back the number stored in that coordinate
search :: Board -> (Int,Int) -> Int
search board coordinates = (board !! ((fst coordinates)-1)) !! ((snd coordinates)-1)

--takes board and column number, gives back a list with column number from up to down.
giveColumn :: Board -> Int -> [Int]
giveColumn [] column = []
giveColumn (x:xs) column = [x !! (column-1)] ++ giveColumn xs column

--gives a list of all the numbers within a square 1-9
giveSquare :: Board -> Int -> [Int]
giveSquare board square
    | elem square [1,4,7] = fst (splitAt 3 (board !! (square-1))) ++ fst (splitAt 3 (board !! (square))) ++ fst (splitAt 3 (board !! (square+1)))
    | elem square [2,5,8] = snd (splitAt 3 (fst (splitAt 6 (board !! (square-2))))) ++ snd (splitAt 3 (fst (splitAt 6 (board !! (square-1))))) ++ snd (splitAt 3 (fst (splitAt 6 (board !! (square)))))
    | elem square [3,6,9] = snd (splitAt 6 (board !! (square-3))) ++ snd (splitAt 6 (board !! (square-2))) ++ snd (splitAt 6 (board !! (square-1)))

-- gives a list of all numbers within a certain row.
giveRow :: Board -> Int -> [Int]
giveRow board row = board !! (row-1)

--find first zero in board and return coordinates.
findZero :: Board -> Int -> (Int,Int)
findZero (x:xs) acc
    | x == [] = findZero xs (acc+1)
    | head x == 0 = ((10-length x), acc)
    | head x /= 0 = findZero ((tail x):xs) acc
