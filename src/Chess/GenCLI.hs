module Chess.GenCLI
  ( genMateN
  ) where

import Chess.Types
import Chess.Board
import Chess.Game
import Chess.Move       (uciOfMove)
import Chess.Apply      (applyMoveRules)
import Chess.Legality   (legalMoves, isCheckmate, isStalemate)
import Chess.Search
  ( matesInExactlyMoves
  , matesInAtMostMoves
  )
import Data.List (nub)

infix 1 .=
(.=) :: Square -> Piece -> (Square, Piece)
(.=) = (,)

-- | Generate KQK mate-in-N puzzles. Scans BK on board edges (corners + files a/h + ranks 1/8)
--   and WK/WQ anywhere. Prints puzzles where White to move mates in EXACTLY N with UNIQUE first move.
genMateN :: Int -> Int -> IO ()
genMateN n want = do
  putStrLn $ "Generating mate-in-" ++ show n ++ " KQK puzzles (target " ++ show want ++ ")..."
  go 0 0 candidates
  where
    allSquaresFR = [ (f,r) | f <- ['a'..'h'], r <- [1..8] ]
    wkSquares = [ at f r | (f,r) <- allSquaresFR ]
    wqSquares = wkSquares

    edges =
      [ (f,r) | f <- ['a','h'], r <- [1..8] ] ++
      [ (f,r) | r <- [1,8],     f <- ['b'..'g'] ]
    bkSquares = [ at f r | (f,r) <- edges ]

    candidates =
      [ startGame (place [wk .= w King, wq .= w Queen, bk .= b King]) White
      | wk <- wkSquares, wq <- wqSquares, bk <- bkSquares
      , distinct [wk,wq,bk]
      , not (kingsAdjacent wk bk)
      , not (isTerminalStart (startGame (place [wk .= w King, wq .= w Queen, bk .= b King]) White))
      ]

    isTerminalStart g = isCheckmate g || isStalemate g

    go found seen [] = putStrLn $ "Done. Scanned " ++ show seen ++ " candidates. Found " ++ show found ++ "."
    go found seen (g:gs)
      | found >= want = putStrLn $ "Done. Reached target " ++ show found ++ "."
      | otherwise =
          case matesInExactlyMoves n g of
            Nothing -> next
            Just pv ->
              if uniqueFirst g n
                then do
                  putStrLn "----------------------------------------"
                  putStrLn (prettyBoard (board g))
                  putStrLn ("White to move. Mate in " ++ show n ++ ".")
                  putStrLn ("PV (UCI): " ++ unwords (map uciOfMove pv))
                  putStrLn "First move is UNIQUE"
                  go (found+1) (seen+1) gs
                else next
      where
        next = do
          whenTick seen
          go found (seen+1) gs

    whenTick k =
      -- lightweight progress every ~10k candidates
      if k `mod` 10000 == 0 then putStrLn ("… scanned " ++ show k ++ " candidates") else pure ()

-- Is the first winning move unique among legal first moves?
uniqueFirst :: Game -> Int -> Bool
uniqueFirst g n =
  length winningFirsts == 1
  where
    ms = legalMoves g
    winningFirsts =
      [ m
      | m <- ms
      , let g' = apply' g m
      , Just _ <- [ matesInAtMostMoves n g' ]
      ]

    apply' st m = case applyMoveRules st m of
                    Right g' -> g'
                    _        -> st   -- legalMoves ensures apply succeeds; safe fallback

-- Helpers
distinct :: Eq a => [a] -> Bool
distinct xs = length xs == length (nub xs)

kingsAdjacent :: Square -> Square -> Bool
kingsAdjacent a b =
  a /= b &&
  abs (fileOf a - fileOf b) <= 1 &&
  abs (rankOf a - rankOf b) <= 1

-- minimal when without importing Control.Monad
when :: Bool -> IO () -> IO ()
when True  act = act
when False _   = pure ()
