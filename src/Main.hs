module Main where

import Game
import Data.Default
import Data.Char
import System.IO

data PlayerCommand = Move Move | TryAgain | Quit

addHelpInfo :: String -> String
addHelpInfo err = err ++ " Try again. If you need help, type 'help' or 'h'."

readMove :: IO PlayerCommand
readMove = do
  s <- getLine
  let s' = map toLower s
  case s' of
    "a1" -> return $ Move A1
    "a2" -> return $ Move A2
    "a3" -> return $ Move A3
    "b1" -> return $ Move B1
    "b2" -> return $ Move B2
    "b3" -> return $ Move B3
    "c1" -> return $ Move C1
    "c2" -> return $ Move C2
    "c3" -> return $ Move C3
    "help" -> getHelp
    "h" -> getHelp
    "quit" -> quit
    "q" -> quit
    _ -> invalidMove
  where
    getHelp = do
      showHelp
      return TryAgain
    invalidMove = do
      putStrLn $ addHelpInfo "Unrecognized move."
      return TryAgain
    quit = do
      putStrLn "Quitting game."
      return Quit


showHelp :: IO ()
showHelp = do
  putStrLn "You are player O, and the computer is player X."
  putStrLn "Every round the grid will be shown. Each cell has its address."
  putStrLn "You should put the address you want to play whenever its your move."
  putStrLn "The address is a two character text."
  putStrLn "The first character is the column (A, B or C)."
  putStrLn "The second character is the line (1, 2 or 3)."
  putStrLn "So in your turn, if you want to play in the middle cell, you should type 'B2' and press Enter."
  putStrLn "If you need help, just type 'help' or 'h' in your turn and press Enter."
  putStrLn "If you want to quit the game, press type 'quit' or 'q' in your turn and press Enter."

endTurn :: Grid -> IO () -> IO ()
endTurn g nextTurn = do
  putStrLn ""
  print g
  putStrLn ""
  case getGameStatus g of
    InProgress -> nextTurn
    Draw -> putStrLn "Game ended. Draw..."
    Winner p -> putStrLn $ "Game ended. Player " ++ show p ++ " wins!!!"

turn :: Player -> Grid -> IO ()
turn O g = do
  let g' = computerMove g
  putStrLn "Computer turn. Making move."
  endTurn g' (turn X g')
turn X g = do
  putStr "Your turn. Choose your move: "
  input <- readMove
  case input of
    TryAgain -> turn X g
    Move m -> case playerMove m g of
      Left err -> inputError err
      Right g' -> endTurn g' (turn O g')
    Quit -> return ()
  where
    inputError err = do
      putStrLn $ addHelpInfo err
      turn X g

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  putStrLn "Tic-tac-toe game!"
  putStrLn ""
  showHelp
  firstPlayer <- getFirstPlayer
  let g = def :: Grid
  putStrLn ""
  print g
  putStrLn ""
  turn firstPlayer g
