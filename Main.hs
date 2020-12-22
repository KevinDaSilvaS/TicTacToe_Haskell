module Main where
import WinCondition (winCondition)
import DisplayGame (display)
import Insert (insert)

game :: [[Char]]
game = [['_','_','_'], ['_','_','_'], ['_','_','_']]

changeCharPlayer :: Char -> Char
changeCharPlayer 'X' = 'O'
changeCharPlayer  _  = 'X'

gameFlow :: Char -> [[Char]] -> IO ()
gameFlow charPlayer gameMatrix = do
  print "type the row you want to insert(0-2)"
  inputRow <- getLine
  let row = (read inputRow :: Int)

  print "type the position you want to insert(0-2)"
  inputPos <- getLine
  let pos = (read inputPos :: Int)

  let nMatrix = insert charPlayer row pos gameMatrix

  if null nMatrix then display gameMatrix else display nMatrix

  if null nMatrix then
    (if '-' `elem` winCondition gameMatrix
    then gameFlow (changeCharPlayer charPlayer) gameMatrix
    else if 'D' `elem` winCondition gameMatrix
    then main
    else print (winCondition gameMatrix))
  else 
    (if '-' `elem` winCondition nMatrix
    then gameFlow (changeCharPlayer charPlayer) nMatrix
    else if 'D' `elem` winCondition nMatrix
    then main
    else print (winCondition nMatrix))

  main

main :: IO ()
main = do
  print "NEW GAME"
  gameFlow 'X' game