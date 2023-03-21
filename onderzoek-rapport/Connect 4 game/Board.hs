module Board  where
import Player
import Data.List (tails, transpose)
import System.Random

data Cell = Empty | Occupied Player deriving (Eq, Show)
type Board = [[Cell]]


initBoard :: Cell -> Board
initBoard n = replicate 6 (replicate 7 Empty)

makeMove :: Board -> Player -> Int -> Board
makeMove board player col = setCell board row col (Occupied player)
  where row = getNextRow board col

getNextRow :: Board -> Int -> Int
getNextRow board col = length (takeWhile (\row -> board !! row !! col /= Empty) [0..5])

setCell :: Board -> Int -> Int -> Cell -> Board
setCell board row col cell = [if i == row then setRow i else board !! i | i <- [0..5]]
  where setRow i = [if j == col then cell else board !! i !! j | j <- [0..6]]


isColumnFull :: Board -> Int -> Bool
isColumnFull board col = board !! (length board - 1) !! col /= Empty

isBoardFull :: Board -> Bool
isBoardFull board = all (\cell -> cell /= Empty) (concat board)


checkWin :: Board -> Player -> Bool
checkWin board player = any winInLine linesToCheck
  where
    rows = board
    cols = transpose board
    linesToCheck = rows ++ cols 
    winInLine line = any (\i -> all (\cell -> cell == Occupied player) (take 4 (drop i line))) [0..length line - 4]

isGameOver :: Board -> Player -> Bool
isGameOver board player = isBoardFull board && not (checkWin board player)

printBoard :: Board -> IO ()
printBoard board = putStrLn (unlines (map showRow (reverse board)))
  where showRow :: [Cell] -> String
        showRow row = concat [showCell x | x <- row] ++ "|"
        showCell :: Cell -> String
        showCell Empty = "| "
        showCell (Occupied player) = "|" ++ show player 

run :: Board -> Player -> IO ()
run board player = do
  printBoard board
  col <- randomRIO (0, 6)
  --putStrLn ("Player " ++ show player ++ ", enter a column number:")
  --colStr <- getLine
  --let col = read colStr :: Int
  let board' = makeMove board player col
  let winner = checkWin board' player
  let draw = isGameOver board' player
  let boardIsfull = isColumnFull board col
  if winner
    then do 
      printBoard board'
      putStrLn ("Player " ++ show player ++ " has won the game!")
  else if draw
    then do
      printBoard board'
      putStrLn "The game resulted in a draw."
  else if boardIsfull
    then do
      putStrLn "This column is full, please choose another column."
      run board player
  else run board' (nextPlayer player)
    