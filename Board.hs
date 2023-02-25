module Board  where
import Player
import Data.List (tails, transpose)

data Cell = Empty | Occupied Player deriving (Eq, Show)
type Board = [[Cell]]


initBoard :: Cell -> Board
initBoard n = replicate 6 (replicate 7 Empty)

makeMove :: Board -> Player -> Int -> Board
makeMove board player col = setCell board row col (Occupied player)
  where row = getNextRow board col

isColumnFull :: Board -> Int -> Bool
isColumnFull board col = board !! 0 !! col /= Empty

getNextRow :: Board -> Int -> Int
getNextRow board col = length (takeWhile (\row -> board !! row !! col /= Empty) [0..5])

setCell :: Board -> Int -> Int -> Cell -> Board
setCell board row col cell = [if i == row then setRow i else board !! i | i <- [0..5]]
  where setRow i = [if j == col then cell else board !! i !! j | j <- [0..6]]

checkWin :: Board -> Player -> Bool
checkWin board player = any winInLine rows || any winInLine cols 
  where
    rows = board
    cols = transpose board
    winInLine line = any (\i -> all (\cell -> cell == Occupied player) (take 4 (drop i line))) [0..length line - 4]

printBoard :: Board -> IO ()
printBoard board = putStrLn (unlines (map showRow (reverse board)))
  where showRow :: [Cell] -> String
        showRow row = concat [showCell x | x <- row] ++ "|"
        showCell :: Cell -> String
        showCell Empty = "| "
        showCell (Occupied player) = show player ++ "|"