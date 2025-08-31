module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Chess.Board    (prettyBoard)
import Chess.Game     (exampleGame, Game(..))
import Chess.Move     (uciToMove)
import Chess.Apply    (applyMoveRules)
import Chess.MoveGen  (perft)
import Chess.Legality (inCheck, isCheckmate, isStalemate)
import Chess.GenCLI   (genMateN, probeMate1)

banner :: String
banner = unlines
  [ "=== Chess Puzzles (Haskell) — CLI ==="
  , ""
  , "Usage:"
  , "  cabal run chess-puzzles -- simulate"
  , "  cabal run chess-puzzles -- perft <depth>"
  , "  cabal run chess-puzzles -- gen mate <N> [COUNT] [--any]"
  , "  cabal run chess-puzzles -- probe mate1"
  , ""
  , "Notes:"
  , "  - UCI moves: e2e4, g7g8Q (promotion)."
  , "  - 'simulate' shows Check/Checkmate/Stalemate after each move."
  , "  - 'gen mate N COUNT [--any]' scans *focused* KQK patterns near the BK."
  , "    '--any' disables the unique-first check."
  , "  - 'probe mate1' runs a known KQK mate-in-1 sanity check."
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

    ["gen", "mate", nStr] ->
      case reads nStr of
        [(n, "")] -> genMateN n 5 False
        _         -> putStrLn "Usage: gen mate <N> [COUNT] [--any]"

    ["gen", "mate", nStr, cntStr] ->
      case (reads nStr, reads cntStr) of
        ([(n, "")], [(k, "")]) -> genMateN n k False
        _                      -> putStrLn "Usage: gen mate <N> [COUNT] [--any]"

    ["gen", "mate", nStr, cntStr, flag] | flag == "--any" ->
      case (reads nStr, reads cntStr) of
        ([(n, "")], [(k, "")]) -> genMateN n k True
        _                      -> putStrLn "Usage: gen mate <N> [COUNT] [--any]"

    ["probe", "mate1"] ->
      probeMate1

    _ -> do
      putStrLn banner
      putStrLn (prettyBoard (board exampleGame))
      putStrLn "White to move in the demo position."

loop :: Game -> Bool -> IO ()
loop g first = do
  putStrLn (prettyBoard (board g))
  if first
    then putStrLn "Enter UCI moves (e2e4, e7e8Q). Type 'q' to quit."
    else pure ()
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
  | isCheckmate g        = "Checkmate."
  | isStalemate g        = "Stalemate."
  | inCheck g (toMove g) = "Check!"
  | otherwise            = ""
