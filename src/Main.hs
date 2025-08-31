module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Chess.Board   (prettyBoard)
import Chess.Game    (exampleGame, Game(..))
import Chess.Move    (uciToMove)
import Chess.Apply   (applyMoveUnsafe)
import Chess.MoveGen (perft)

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

helpText :: Game -> String
helpText g = unlines
  [ prettyBoard (board g)
  , "Commands:"
  , "  • Enter a UCI move (e.g., e2e4, b7b8Q)."
  , "  • Type 'help' to see this message again."
  , "  • Type 'q' or 'quit' to exit."
  , ""
  , "Notes:"
  , "  • This simulator is minimal: no full legality checks yet."
  , "  • Promotions: append piece letter at the end (Q R B N), e.g., e7e8Q."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["simulate"] ->
      loop exampleGame True
    ["perft", dStr] ->
      case reads dStr of
        [(d, "")] -> putStrLn ("nodes=" ++ show (perft d exampleGame))
        _         -> putStrLn "Usage: perft <depth>"
    _ -> do
      putStrLn banner
      putStrLn (prettyBoard (board exampleGame))
      putStrLn "White to move in the demo position."

loop :: Game -> Bool -> IO ()
loop g first = do
  if first
    then putStrLn (helpText g)
    else putStrLn (prettyBoard (board g))
  putStr (show (toMove g) ++ " to move — enter UCI (e2e4, e7e8Q), 'help' or 'q': ")
  hFlush stdout
  ln <- getLine
  case ln of
    "q"    -> putStrLn "Bye!"
    "quit" -> putStrLn "Bye!"
    "help" -> loop g True
    _ ->
      case uciToMove ln of
        Nothing -> putStrLn "Could not parse that move. Type 'help' for examples." >> loop g False
        Just mv ->
          case applyMoveUnsafe g mv of
            Left msg -> putStrLn ("Rejected: " ++ msg) >> loop g False
            Right g' -> loop g' False
