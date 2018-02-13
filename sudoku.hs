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

giveSquare :: Board -> Int -> [Int]
giveSquare board square
    | elem square [1,2,3] = [(splitAt (square+1) (board !! 0))] ++ [(splitAt (square+1) (board !! 1))] ++ [(splitAt (square+1) (board !! 2))]
    | elem square [4,5,6] = [(splitAt (square-2) (board !! 3))] ++ [(splitAt (square-2) (board !! 4))] ++ [(splitAt (square-2) (board !! 5))]
    | elem square [7,8,9] = [(splitAt (square-5) (board !! 6))] ++ [(splitAt (square-5) (board !! 7))] ++ [(splitAt (square-5) (board !! 8))]
