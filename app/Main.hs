module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Chess.Board   (prettyBoard)
import Chess.Game    (exampleGame, Game(..))
import Chess.Move    (uciToMove)
import Chess.Apply   (applyMoveRules)
import Chess.MoveGen (perft)
import Chess.Legality (inCheck, isCheckmate, isStalemate)

banner :: String
banner = unlines
  [ "=== Chess Puzzles (Haskell) — Demo ==="
  , "Below is the current demo position."
  , "- White pieces are uppercase (K Q R B N P); black are lowercase."
  , "- '.' marks an empty square. Ranks 8→1, files a→h."
  , ""
  , "Run interactive simulation:"
  , "  cabal run chess-puzzles -- simulate"
  , ""
  , "Perft (pseudo-legal node count):"
  , "  cabal run chess-puzzles -- perft 2"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["simulate"]       -> loop exampleGame True
    ["perft", dStr]    -> case reads dStr of
                            [(d,"")] -> putStrLn ("nodes=" ++ show (perft d exampleGame))
                            _        -> putStrLn "Usage: perft <depth>"
    _                  -> do
      putStrLn banner
      putStrLn (prettyBoard (board exampleGame))
      putStrLn "White to move in the demo position."

loop :: Game -> Bool -> IO ()
loop g first = do
  putStrLn (prettyBoard (board g))
  if first then putStrLn "Type moves like e2e4, g7g8Q; 'q' to quit." else pure ()
  putStr (show (toMove g) ++ " to move — enter UCI: ")
  hFlush stdout
  ln <- getLine
  case ln of
    "q"    -> putStrLn "Bye!"
    "quit" -> putStrLn "Bye!"
    _      -> case uciToMove ln of
                Nothing -> putStrLn "Could not parse that move." >> loop g False
                Just mv -> case applyMoveRules g mv of
                             Left msg -> putStrLn ("Rejected: " ++ msg) >> loop g False
                             Right g' -> do
                               putStrLn (status g')
                               loop g' False

status :: Game -> String
status g
  | isCheckmate g = "Checkmate."
  | isStalemate g = "Stalemate."
  | inCheck g (toMove g) = "Check!"
  | otherwise = ""
