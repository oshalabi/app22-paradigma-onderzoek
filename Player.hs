module Player where 

import System.Random

data Player = X | O deriving (Eq, Show)

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

chooseRandomPlayer :: IO Player
chooseRandomPlayer = do
  randomNum <- randomRIO (0, 1) :: IO Int
  return $ if randomNum <= 0 then X else O

