module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Chess.Board    (prettyBoard)
import Chess.Game     (exampleGame, Game(..))
import Chess.Move     (uciToMove)
import Chess.Apply    (applyMoveRules)
import Chess.MoveGen  (perft)
import Chess.Legality (inCheck, isCheckmate, isStalemate)
import Chess.GenCLI   (genMateN, probeMate1, genMateRandom)

banner :: String
banner = unlines
  [ "=== Chess Puzzles (Haskell) - CLI ==="
  , ""
  , "Usage:"
  , "  cabal run chess-puzzles -- simulate"
  , "  cabal run chess-puzzles -- perft <depth>"
  , "  cabal run chess-puzzles -- gen mate <N> [COUNT] [--any]"
  , "  cabal run chess-puzzles -- gen random <PIECES> <N> [COUNT] [--any] [--tries T]"
  , "  cabal run chess-puzzles -- probe mate1"
  , ""
  , "Notes:"
  , "  - UCI moves: e2e4, g7g8Q (promotion)."
  , "  - 'gen random' places two kings plus (PIECES-2) random pieces, White to move."
  , "  - '--any' disables the unique-first requirement."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["simulate"] ->
      loop exampleGame True

    ["perft", dStr] ->
      case readMaybe dStr of
        Just d  -> putStrLn ("nodes=" ++ show (perft d exampleGame))
        Nothing -> putStrLn "Usage: perft <depth>"

    ("gen":"mate":rest) ->
      case parseGenMate rest of
        Right (n, count, allowAny) -> genMateN n count allowAny
        Left err                   -> putStrLn err

    ("gen":"random":rest) ->
      case parseGenRandom rest of
        Right (pieces, n, count, allowAny, tries) ->
          genMateRandom pieces n count allowAny tries
        Left err -> putStrLn err

    ["probe", "mate1"] ->
      probeMate1

    _ -> do
      putStrLn banner
      putStrLn (prettyBoard (board exampleGame))
      putStrLn "White to move in the demo position."

-- Interactive loop for 'simulate'
loop :: Game -> Bool -> IO ()
loop g first = do
  putStrLn (prettyBoard (board g))
  if first
    then putStrLn "Enter UCI moves (e2e4, e7e8Q). Type 'q' to quit."
    else pure ()
  putStr (show (toMove g) ++ " to move - enter UCI: ")
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

-- =========================
-- CLI parsers
-- =========================

-- gen mate <N> [COUNT] [--any]
parseGenMate :: [String] -> Either String (Int, Int, Bool)
parseGenMate xs =
  case xs of
    (nStr:rest) ->
      case readMaybe nStr of
        Nothing -> Left "Usage: gen mate <N> [COUNT] [--any]"
        Just n  ->
          let defaults = (5 :: Int, False) -- count, allowAny
          in case parseMateTail defaults rest of
               Right (count, allowAny) -> Right (n, count, allowAny)
               Left e                  -> Left e
    _ -> Left "Usage: gen mate <N> [COUNT] [--any]"

parseMateTail :: (Int, Bool) -> [String] -> Either String (Int, Bool)
parseMateTail st [] = Right st
parseMateTail (cnt, anyF) ("--any":xs) = parseMateTail (cnt, True) xs
parseMateTail (cnt, anyF) (tok:xs) =
  case readMaybe tok of
    Just c | cnt == 5 -> parseMateTail (c, anyF) xs
    _                 -> Left "Usage: gen mate <N> [COUNT] [--any]"

-- gen random <PIECES> <N> [COUNT] [--any] [--tries T]
parseGenRandom :: [String] -> Either String (Int, Int, Int, Bool, Int)
parseGenRandom xs =
  case xs of
    (pStr:nStr:rest) ->
      case (readMaybe pStr, readMaybe nStr) of
        (Just p, Just n) ->
          let defaults = (Nothing :: Maybe Int, False, 20000 :: Int)
          in case parseRandomTail defaults rest of
               Right (mCnt, allowAny, tries) ->
                 let count = maybe 5 id mCnt
                 in Right (p, n, count, allowAny, tries)
               Left e -> Left e
        _ -> Left "Usage: gen random <PIECES> <N> [COUNT] [--any] [--tries T]"
    _ -> Left "Usage: gen random <PIECES> <N> [COUNT] [--any] [--tries T]"

-- state: (maybeCount, allowAny, tries)
parseRandomTail :: (Maybe Int, Bool, Int) -> [String] -> Either String (Maybe Int, Bool, Int)
parseRandomTail st [] = Right st
parseRandomTail (mCnt, anyF, tr) ("--any":xs) = parseRandomTail (mCnt, True, tr) xs
parseRandomTail (mCnt, anyF, tr) ("--tries":tStr:xs) =
  case readMaybe tStr of
    Just t  -> parseRandomTail (mCnt, anyF, t) xs
    Nothing -> Left "Invalid value for --tries"
parseRandomTail st ("--tries":[]) = Left "Missing value for --tries"
-- accept positional COUNT if not already set
parseRandomTail (Nothing, anyF, tr) (tok:xs) =
  case readMaybe tok of
    Just c  -> parseRandomTail (Just c, anyF, tr) xs
    Nothing -> Left "Unrecognized token. Usage: gen random <PIECES> <N> [COUNT] [--any] [--tries T]"
-- if COUNT already set and we see another bare token, reject
parseRandomTail (_mCnt, _anyF, _tr) (_:_) =
  Left "Unrecognized token. Usage: gen random <PIECES> <N> [COUNT] [--any] [--tries T]"
