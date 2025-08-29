module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Chess.Board (prettyBoard)
import Chess.Game  (exampleGame, Game(..))
import Chess.Move  (uciToMove)
import Chess.Apply (applyMoveUnsafe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["simulate"] -> loop exampleGame
    _             -> putStrLn (prettyBoard (board exampleGame))

loop :: Game -> IO ()
loop g = do
  putStrLn (prettyBoard (board g))
  putStr (show (toMove g) ++ " to move; enter UCI (e2e4, e7e8Q) or 'q': ")
  hFlush stdout
  ln <- getLine
  if ln == "q" || ln == "quit"
    then putStrLn "Bye!"
    else case uciToMove ln of
           Nothing -> putStrLn "Could not parse that move." >> loop g
           Just mv -> case applyMoveUnsafe g mv of
                        Left msg -> putStrLn ("Rejected: " ++ msg) >> loop g
                        Right g' -> loop g'
