import Data.List
import System.Random

type Grid = [[Char]]

initialGrid :: Grid
initialGrid = replicate 6 (replicate 7 '.')

putPiece :: Grid -> Int -> Char -> Grid
putPiece grid col piece = [take col row ++ [piece] ++ drop (col + 1) row | row <- grid]

dropPiece :: Grid -> Int -> Grid
dropPiece grid col =
  let (row:rows) = dropWhile (\row -> row !! col == '.') grid
  in putPiece (row:rows) col '.'

printGrid :: Grid -> IO ()
printGrid grid = putStrLn (concat [row ++ "\n" | row <- grid])

play :: Grid -> Char -> IO ()
play grid player = do
  let grid' = foldl (flip $ putPiece grid) initialGrid [0..6]
  printGrid grid'
  if winner grid' /= '.' then
    putStrLn [winner grid', "wins!"]
  else
    play grid' (if player == 'X' then 'O' else 'X')

winner :: Grid -> Char
winner grid =
  let lines = grid ++ transpose grid ++ diagonals grid
      won = any (\line -> all (\piece -> piece == 'X') line || all (\piece -> piece == 'O') line) lines
  in if won then 'X' else '.'

diagonals :: Grid -> [[Char]]
diagonals grid =
  let indices = [([0,0],[1,1]),([0,6],[1,-1]),([6,0],[-1,1]),([6,6],[-1,-1])]
  in concat [[grid !! (r + dr * i) !! (c + dc * i) | i <- [0..3]] | (r,c) <- indices, let (dr,dc) = rc]

main :: IO ()
main = do
  g <- newStdGen
  let moves = randomRs (0,6) g :: [Int]
  play initialGrid 'X'