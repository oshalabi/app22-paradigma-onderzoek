import Board
import Player
import System.Random


main :: IO ()
main = do
  let board = initBoard Empty
  run board X

run :: Board -> Player -> IO ()
run board player = do
  printBoard board
  col <- randomRIO (0, 6)
  --putStrLn ("Player " ++ show player ++ ", enter a column number:")
  --colStr <- getLine
  --let col = read colStr :: Int
  if isColumnFull board col
    then do
      putStrLn "This column is full, please choose another column."
      run board player
    else do
      let board' = makeMove board player col
      let winner = checkWin board' player
      if winner 
        then do
          printBoard board'
          putStrLn ("Player " ++ show player ++ " has won the game!")
      else run board' (nextPlayer player)



nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

