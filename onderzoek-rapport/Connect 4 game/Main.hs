import Board
import Player


main :: IO ()
main = do
  let board = initBoard Empty
  player <- chooseRandomPlayer
  run board player