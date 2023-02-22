import Board
import Player


main :: IO ()
main = do
  let board = initBoard Empty
  run board X

run :: Board -> Player -> IO ()
run board player = do
  printBoard board
  putStrLn ("Player " ++ show player ++ ", enter a column number:")
  colStr <- getLine
  let col = read colStr :: Int
  let board' = makeMove board player col
  if checkWin board' player
    then putStrLn ("Player " ++ show player ++ " has won the game!")
    else run board' (nextPlayer player)


nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

