module Chess.GenCLI
  ( genMateN
  , probeMate1
  ) where

import Chess.Types
import Chess.Board
import Chess.Game
import Chess.Move       (uciOfMove)
import Chess.Apply      (applyMoveRules)
import Chess.Legality   (legalMoves, isCheckmate, isStalemate)
import Chess.Search     (matesInExactlyMoves, matesInAtMostMoves)
import Data.List        (nub)

infix 1 .=
(.=) :: Square -> Piece -> (Square, Piece)
(.=) = (,)

-- | Chebyshev distance (king metric).
cheb :: Square -> Square -> Int
cheb a b = max (abs (fileOf a - fileOf b)) (abs (rankOf a - rankOf b))

-- | Generate KQK mate-in-N puzzles with a *focused* scan:
--   - black king ANY square
--   - white king at Chebyshev distance exactly 2 from BK (not adjacent)
--   - white queen within Chebyshev distance 1..3 from BK
--   This keeps candidates realistic and fast.
--
--   allowAny = True  → accept puzzles even if multiple winning first moves exist
--   allowAny = False → require a unique winning first move
genMateN :: Int -> Int -> Bool -> IO ()
genMateN n want allowAny = do
  putStrLn $ "Generating mate-in-" ++ show n ++ " (focused KQK), target " ++ show want ++ "…"
  go 0 0 candidates
  where
    allSquares = [ at f r | f <- ['a'..'h'], r <- [1..8] ]
    bkSquares  = allSquares

    -- for each BK, choose WK at distance exactly 2 (and not adjacent),
    -- and WQ within distance 1..3 from BK
    candidates =
      [ startGame (place [wk .= w King, wq .= w Queen, bk .= b King]) White
      | bk <- bkSquares
      , wk <- [ s | s <- allSquares, s /= bk, cheb s bk == 2 ]
      , wq <- [ s | s <- allSquares, s /= bk, s /= wk, let d = cheb s bk, d >= 1, d <= 3 ]
      , not (isTerminalStart (startGame (place [wk .= w King, wq .= w Queen, bk .= b King]) White))
      ]

    isTerminalStart g = isCheckmate g || isStalemate g

    go found seen [] =
      putStrLn $ "Done. Scanned " ++ show seen ++ " candidates. Found " ++ show found ++ "."
    go found seen (g:gs)
      | found >= want = putStrLn $ "Done. Reached target " ++ show found ++ "."
      | otherwise =
          case matesInExactlyMoves n g of
            Nothing -> tick >> go found (seen+1) gs
            Just pv ->
              if allowAny || uniqueFirst g n
                then do
                  putStrLn "----------------------------------------"
                  putStrLn (prettyBoard (board g))
                  putStrLn ("White to move. Mate in " ++ show n ++ ".")
                  putStrLn ("PV (UCI): " ++ unwords (map uciOfMove pv))
                  tick
                  go (found+1) (seen+1) gs
                else tick >> go found (seen+1) gs
      where
        tick = whenTick seen

    whenTick k =
      if k `mod` 5000 == 0 then putStrLn ("… scanned " ++ show k ++ " candidates") else pure ()

-- Is the first winning move unique among all legal first moves?
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
                    _        -> st   -- legalMoves should guarantee success

-- === Sanity probe: a known KQK mate-in-1 pattern ===
-- BK a8, WK c7, WQ c6 → 1.Qb7# is mate
probeMate1 :: IO ()
probeMate1 = do
  let g0 = startGame (place
           [ at 'a' 8 .= b King
           , at 'c' 7 .= w King
           , at 'c' 6 .= w Queen
           ]) White
  putStrLn "Known KQK mate-in-1 probe:"
  putStrLn (prettyBoard (board g0))
  case matesInExactlyMoves 1 g0 of
    Nothing -> putStrLn "Solver did NOT find mate-in-1 here (unexpected)."
    Just pv -> do
      putStrLn ("Solver PV: " ++ unwords (map uciOfMove pv))
      putStrLn "Expected first move among PV: Qc6b7#"

-- tiny 'when' without importing Control.Monad
when :: Bool -> IO () -> IO ()
when True  act = act
when False _   = pure ()
