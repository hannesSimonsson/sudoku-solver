--example board
--[[0,1,0],
-- [1,2,3],
-- [2,0,0]]
type Board = [[Int]]

--sudoku :: Board -> Board
--sudoku board
--    | notElem 0 (concat Board) = Board
--    | elem 0 (concat Board) = solve

-- search takes the board and a cordinate and gives back the number stored in that cordinate
search :: Board -> (Int,Int) -> Int
search board cordinates = (board !! fst cordinates) !! snd cordinates

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
