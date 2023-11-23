module Game where

import Data.Default
import System.Random

data Player = X | O
  deriving (Show, Eq)

data Mark = Empty | Mark Player
  deriving (Eq)

instance Default Mark where
  def = Empty

instance Show Mark where
  show Empty    = " "
  show (Mark m) = show m

data Grid = Grid Mark Mark Mark Mark Mark Mark Mark Mark Mark
 deriving (Eq)

data Move = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3
  deriving (Show, Eq)

instance Default Grid where
  def = Grid def def def def def def def def def

instance Show Grid where
  show (Grid a1 b1 c1 a2 b2 c2 a3 b3 c3) =
    "   A  B  C\n1  " ++
    show a1 ++ "  " ++ show b1 ++ "  " ++ show c1 ++ "\n2  " ++
    show a2 ++ "  " ++ show b2 ++ "  " ++ show c2 ++ "\n3  " ++
    show a3 ++ "  " ++ show b3 ++ "  " ++ show c3

data GameStatus = InProgress | Draw | Winner Player
  deriving (Eq, Show)

getFirstPlayer :: IO Player
getFirstPlayer = do
  randomBool <- generateRandom :: IO Bool
  return $ if randomBool then X else O
  where
    generateRandom = fst . random <$> getStdGen

playerMove :: Move -> Grid -> Either String Grid
playerMove A1 (Grid Empty b1 c1 a2 b2 c2 a3 b3 c3) = Right (Grid (Mark X) b1 c1 a2 b2 c2 a3 b3 c3)
playerMove B1 (Grid a1 Empty c1 a2 b2 c2 a3 b3 c3) = Right (Grid a1 (Mark X) c1 a2 b2 c2 a3 b3 c3)
playerMove C1 (Grid a1 b1 Empty a2 b2 c2 a3 b3 c3) = Right (Grid a1 b1 (Mark X) a2 b2 c2 a3 b3 c3)
playerMove A2 (Grid a1 b1 c1 Empty b2 c2 a3 b3 c3) = Right (Grid a1 b1 c1 (Mark X) b2 c2 a3 b3 c3)
playerMove B2 (Grid a1 b1 c1 a2 Empty c2 a3 b3 c3) = Right (Grid a1 b1 c1 a2 (Mark X) c2 a3 b3 c3)
playerMove C2 (Grid a1 b1 c1 a2 b2 Empty a3 b3 c3) = Right (Grid a1 b1 c1 a2 b2 (Mark X) a3 b3 c3)
playerMove A3 (Grid a1 b1 c1 a2 b2 c2 Empty b3 c3) = Right (Grid a1 b1 c1 a2 b2 c2 (Mark X) b3 c3)
playerMove B3 (Grid a1 b1 c1 a2 b2 c2 a3 Empty c3) = Right (Grid a1 b1 c1 a2 b2 c2 a3 (Mark X) c3)
playerMove C3 (Grid a1 b1 c1 a2 b2 c2 a3 b3 Empty) = Right (Grid a1 b1 c1 a2 b2 c2 a3 b3 (Mark X))
playerMove _ _ = Left "Invalid move. Please try again."

computerMove :: Grid -> Grid
computerMove (Grid (Mark O) (Mark O) Empty a2 b2 c2 a3 b3 c3) = Grid (Mark O) (Mark O) (Mark O) a2 b2 c2 a3 b3 c3
computerMove (Grid (Mark O) Empty (Mark O) a2 b2 c2 a3 b3 c3) = Grid (Mark O) (Mark O) (Mark O) a2 b2 c2 a3 b3 c3
computerMove (Grid Empty (Mark O) (Mark O) a2 b2 c2 a3 b3 c3) = Grid (Mark O) (Mark O) (Mark O) a2 b2 c2 a3 b3 c3
computerMove (Grid a1 b1 c1 (Mark O) (Mark O) Empty a3 b3 c3) = Grid a1 b1 c1 (Mark O) (Mark O) (Mark O) a3 b3 c3
computerMove (Grid a1 b1 c1 (Mark O) Empty (Mark O) a3 b3 c3) = Grid a1 b1 c1 (Mark O) (Mark O) (Mark O) a3 b3 c3
computerMove (Grid a1 b1 c1 Empty (Mark O) (Mark O) a3 b3 c3) = Grid a1 b1 c1 (Mark O) (Mark O) (Mark O) a3 b3 c3
computerMove (Grid a1 b1 c1 a2 b2 c2 (Mark O) (Mark O) Empty) = Grid a1 b1 c1 a2 b2 c2 (Mark O) (Mark O) (Mark O)
computerMove (Grid a1 b1 c1 a2 b2 c2 (Mark O) Empty (Mark O)) = Grid a1 b1 c1 a2 b2 c2 (Mark O) (Mark O) (Mark O)
computerMove (Grid a1 b1 c1 a2 b2 c2 Empty (Mark O) (Mark O)) = Grid a1 b1 c1 a2 b2 c2 (Mark O) (Mark O) (Mark O)
computerMove (Grid Empty b1 c1 a2 (Mark O) c2 a3 b3 (Mark O)) = Grid (Mark O) b1 c1 a2 (Mark O) c2 a3 b3 (Mark O)
computerMove (Grid (Mark O) b1 c1 a2 Empty c2 a3 b3 (Mark O)) = Grid (Mark O) b1 c1 a2 (Mark O) c2 a3 b3 (Mark O)
computerMove (Grid (Mark O) b1 c1 a2 (Mark O) c2 a3 b3 Empty) = Grid (Mark O) b1 c1 a2 (Mark O) c2 a3 b3 (Mark O)
computerMove (Grid a1 b1 Empty a2 (Mark O) c2 (Mark O) b3 c3) = Grid a1 b1 (Mark O) a2 (Mark O) c2 (Mark O) b3 c3
computerMove (Grid a1 b1 (Mark O) a2 Empty c2 (Mark O) b3 c3) = Grid a1 b1 (Mark O) a2 (Mark O) c2 (Mark O) b3 c3
computerMove (Grid a1 b1 (Mark O) a2 (Mark O) c2 Empty b3 c3) = Grid a1 b1 (Mark O) a2 (Mark O) c2 (Mark O) b3 c3
computerMove (Grid Empty b1 c1 (Mark O) b2 c2 (Mark O) b3 c3) = Grid (Mark O) b1 c1 (Mark O) b2 c2 (Mark O) b3 c3
computerMove (Grid (Mark O) b1 c1 Empty b2 c2 (Mark O) b3 c3) = Grid (Mark O) b1 c1 (Mark O) b2 c2 (Mark O) b3 c3
computerMove (Grid (Mark O) b1 c1 (Mark O) b2 c2 Empty b3 c3) = Grid (Mark O) b1 c1 (Mark O) b2 c2 (Mark O) b3 c3
computerMove (Grid a1 Empty c1 a2 (Mark O) c2 a3 (Mark O) c3) = Grid a1 (Mark O) c1 a2 (Mark O) c2 a3 (Mark O) c3
computerMove (Grid a1 (Mark O) c1 a2 Empty c2 a3 (Mark O) c3) = Grid a1 (Mark O) c1 a2 (Mark O) c2 a3 (Mark O) c3
computerMove (Grid a1 (Mark O) c1 a2 (Mark O) c2 a3 Empty c3) = Grid a1 (Mark O) c1 a2 (Mark O) c2 a3 (Mark O) c3
computerMove (Grid a1 b1 Empty a2 b2 (Mark O) a3 b3 (Mark O)) = Grid a1 b1 (Mark O) a2 b2 (Mark O) a3 b3 (Mark O)
computerMove (Grid a1 b1 (Mark O) a2 b2 Empty a3 b3 (Mark O)) = Grid a1 b1 (Mark O) a2 b2 (Mark O) a3 b3 (Mark O)
computerMove (Grid a1 b1 (Mark O) a2 b2 (Mark O) a3 b3 Empty) = Grid a1 b1 (Mark O) a2 b2 (Mark O) a3 b3 (Mark O)
computerMove (Grid (Mark X) (Mark X) Empty a2 b2 c2 a3 b3 c3) = Grid (Mark X) (Mark X) (Mark O) a2 b2 c2 a3 b3 c3
computerMove (Grid (Mark X) Empty (Mark X) a2 b2 c2 a3 b3 c3) = Grid (Mark X) (Mark O) (Mark X) a2 b2 c2 a3 b3 c3
computerMove (Grid Empty (Mark X) (Mark X) a2 b2 c2 a3 b3 c3) = Grid (Mark O) (Mark X) (Mark X) a2 b2 c2 a3 b3 c3
computerMove (Grid a1 b1 c1 (Mark X) (Mark X) Empty a3 b3 c3) = Grid a1 b1 c1 (Mark X) (Mark X) (Mark O) a3 b3 c3
computerMove (Grid a1 b1 c1 (Mark X) Empty (Mark X) a3 b3 c3) = Grid a1 b1 c1 (Mark X) (Mark O) (Mark X) a3 b3 c3
computerMove (Grid a1 b1 c1 Empty (Mark X) (Mark X) a3 b3 c3) = Grid a1 b1 c1 (Mark O) (Mark X) (Mark X) a3 b3 c3
computerMove (Grid a1 b1 c1 a2 b2 c2 (Mark X) (Mark X) Empty) = Grid a1 b1 c1 a2 b2 c2 (Mark X) (Mark X) (Mark O)
computerMove (Grid a1 b1 c1 a2 b2 c2 (Mark X) Empty (Mark X)) = Grid a1 b1 c1 a2 b2 c2 (Mark X) (Mark O) (Mark X)
computerMove (Grid a1 b1 c1 a2 b2 c2 Empty (Mark X) (Mark X)) = Grid a1 b1 c1 a2 b2 c2 (Mark O) (Mark X) (Mark X)
computerMove (Grid Empty b1 c1 a2 (Mark X) c2 a3 b3 (Mark X)) = Grid (Mark O) b1 c1 a2 (Mark X) c2 a3 b3 (Mark X)
computerMove (Grid (Mark X) b1 c1 a2 Empty c2 a3 b3 (Mark X)) = Grid (Mark X) b1 c1 a2 (Mark O) c2 a3 b3 (Mark X)
computerMove (Grid (Mark X) b1 c1 a2 (Mark X) c2 a3 b3 Empty) = Grid (Mark X) b1 c1 a2 (Mark X) c2 a3 b3 (Mark O)
computerMove (Grid a1 b1 Empty a2 (Mark X) c2 (Mark X) b3 c3) = Grid a1 b1 (Mark O) a2 (Mark X) c2 (Mark X) b3 c3
computerMove (Grid a1 b1 (Mark X) a2 Empty c2 (Mark X) b3 c3) = Grid a1 b1 (Mark X) a2 (Mark O) c2 (Mark X) b3 c3
computerMove (Grid a1 b1 (Mark X) a2 (Mark X) c2 Empty b3 c3) = Grid a1 b1 (Mark X) a2 (Mark X) c2 (Mark O) b3 c3
computerMove (Grid Empty b1 c1 (Mark X) b2 c2 (Mark X) b3 c3) = Grid (Mark O) b1 c1 (Mark X) b2 c2 (Mark X) b3 c3
computerMove (Grid (Mark X) b1 c1 Empty b2 c2 (Mark X) b3 c3) = Grid (Mark X) b1 c1 (Mark O) b2 c2 (Mark X) b3 c3
computerMove (Grid (Mark X) b1 c1 (Mark X) b2 c2 Empty b3 c3) = Grid (Mark X) b1 c1 (Mark X) b2 c2 (Mark O) b3 c3
computerMove (Grid a1 Empty c1 a2 (Mark X) c2 a3 (Mark X) c3) = Grid a1 (Mark O) c1 a2 (Mark X) c2 a3 (Mark X) c3
computerMove (Grid a1 (Mark X) c1 a2 Empty c2 a3 (Mark X) c3) = Grid a1 (Mark X) c1 a2 (Mark O) c2 a3 (Mark X) c3
computerMove (Grid a1 (Mark X) c1 a2 (Mark X) c2 a3 Empty c3) = Grid a1 (Mark X) c1 a2 (Mark X) c2 a3 (Mark O) c3
computerMove (Grid a1 b1 Empty a2 b2 (Mark X) a3 b3 (Mark X)) = Grid a1 b1 (Mark O) a2 b2 (Mark X) a3 b3 (Mark X)
computerMove (Grid a1 b1 (Mark X) a2 b2 Empty a3 b3 (Mark X)) = Grid a1 b1 (Mark X) a2 b2 (Mark O) a3 b3 (Mark X)
computerMove (Grid a1 b1 (Mark X) a2 b2 (Mark X) a3 b3 Empty) = Grid a1 b1 (Mark X) a2 b2 (Mark X) a3 b3 (Mark O)
computerMove (Grid a1 b1 c1 a2 Empty c2 a3 b3 c3) = Grid a1 b1 c1 a2 (Mark O) c2 a3 b3 c3
computerMove (Grid Empty b1 c1 a2 b2 c2 a3 b3 c3) = Grid (Mark O) b1 c1 a2 b2 c2 a3 b3 c3
computerMove (Grid a1 b1 Empty a2 b2 c2 a3 b3 c3) = Grid a1 b1 (Mark O) a2 b2 c2 a3 b3 c3
computerMove (Grid a1 b1 c1 a2 b2 c2 Empty b3 c3) = Grid a1 b1 c1 a2 b2 c2 (Mark O) b3 c3
computerMove (Grid a1 b1 c1 a2 b2 c2 a3 b3 Empty) = Grid a1 b1 c1 a2 b2 c2 a3 b3 (Mark O)
computerMove (Grid a1 Empty c1 a2 b2 c2 a3 b3 c3) = Grid a1 (Mark O) c1 a2 b2 c2 a3 b3 c3
computerMove (Grid a1 b1 c1 Empty b2 c2 a3 b3 c3) = Grid a1 b1 c1 (Mark O) b2 c2 a3 b3 c3
computerMove (Grid a1 b1 c1 a2 b2 Empty a3 b3 c3) = Grid a1 b1 c1 a2 b2 (Mark O) a3 b3 c3
computerMove (Grid a1 b1 c1 a2 b2 c2 a3 Empty c3) = Grid a1 b1 c1 a2 b2 c2 a3 (Mark O) c3
computerMove _ = error "Game has ended!"

getGameStatus :: Grid -> GameStatus
getGameStatus (Grid (Mark X) (Mark X) (Mark X) _ _ _ _ _ _) = Winner X
getGameStatus (Grid _ _ _ (Mark X) (Mark X) (Mark X) _ _ _) = Winner X
getGameStatus (Grid _ _ _ _ _ _ (Mark X) (Mark X) (Mark X)) = Winner X
getGameStatus (Grid (Mark X) _ _ (Mark X) _ _ (Mark X) _ _) = Winner X
getGameStatus (Grid _ (Mark X) _ _ (Mark X) _ _ (Mark X) _) = Winner X
getGameStatus (Grid _ _ (Mark X) _ _ (Mark X) _ _ (Mark X)) = Winner X
getGameStatus (Grid (Mark X) _ _ _ (Mark X) _ _ _ (Mark X)) = Winner X
getGameStatus (Grid _ _ (Mark X) _ (Mark X) _ (Mark X) _ _) = Winner X
getGameStatus (Grid (Mark O) (Mark O) (Mark O) _ _ _ _ _ _) = Winner O
getGameStatus (Grid _ _ _ (Mark O) (Mark O) (Mark O) _ _ _) = Winner O
getGameStatus (Grid _ _ _ _ _ _ (Mark O) (Mark O) (Mark O)) = Winner O
getGameStatus (Grid (Mark O) _ _ (Mark O) _ _ (Mark O) _ _) = Winner O
getGameStatus (Grid _ (Mark O) _ _ (Mark O) _ _ (Mark O) _) = Winner O
getGameStatus (Grid _ _ (Mark O) _ _ (Mark O) _ _ (Mark O)) = Winner O
getGameStatus (Grid (Mark O) _ _ _ (Mark O) _ _ _ (Mark O)) = Winner O
getGameStatus (Grid _ _ (Mark O) _ (Mark O) _ (Mark O) _ _) = Winner O
getGameStatus (Grid (Mark _) (Mark _) (Mark _) (Mark _) (Mark _) (Mark _) (Mark _) (Mark _) (Mark _)) = Draw
getGameStatus _ = InProgress
