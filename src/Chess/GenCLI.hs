module Chess.GenCLI
  ( genMateN
  , probeMate1
  , genMateRandom
  ) where

import Chess.Types
import Chess.Board
import Chess.Game
import Chess.Move       (uciOfMove)
import Chess.Apply      (applyMoveRules)
import Chess.Legality   (legalMoves, isCheckmate, isStalemate)
import Chess.Search     (matesInExactlyMoves, matesInAtMostMoves)
import Data.List        (nub, delete)
import System.Random    (randomRIO)

infix 1 .=
(.=) :: Square -> Piece -> (Square, Piece)
(.=) = (,)

-- ======================================================
-- Utilities
-- ======================================================

-- Chebyshev (king) distance.
cheb :: Square -> Square -> Int
cheb a b = max (abs (fileOf a - fileOf b)) (abs (rankOf a - rankOf b))

allSquares :: [Square]
allSquares = [ Square i | i <- [0..63] ]

-- Random helpers
pickOne :: [a] -> IO a
pickOne xs = do
  i <- randomRIO (0, length xs - 1)
  pure (xs !! i)

-- Note the Eq constraint is required because we use 'delete'.
pickNNoRepeat :: Eq a => Int -> [a] -> IO [a]
pickNNoRepeat n = go n []
  where
    go 0 acc _  = pure (reverse acc)
    go _ acc [] = pure (reverse acc)
    go k acc xs = do
      x <- pickOne xs
      go (k-1) (x:acc) (delete x xs)

-- ======================================================
-- Focused KQK generator
-- ======================================================

genMateN :: Int -> Int -> Bool -> IO ()
genMateN n want allowAny = do
  putStrLn $ "Generating mate-in-" ++ show n ++ " (focused KQK), target " ++ show want ++ "..."
  go 0 0 candidates
  where
    bkSquares  = allSquares
    candidates =
      [ startGame (place [wk .= w King, wq .= w Queen, bk .= b King]) White
      | bk <- bkSquares
      , wk <- [ s | s <- allSquares, s /= bk, cheb s bk == 2 ]
      , wq <- [ s | s <- allSquares, s /= bk, s /= wk
                  , let d = cheb s bk, d >= 1, d <= 3 ]
      , let g0 = startGame (place [wk .= w King, wq .= w Queen, bk .= b King]) White
      , not (isTerminalStart g0)
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
      if k `mod` 5000 == 0 then putStrLn ("scanned " ++ show k ++ " candidates") else pure ()

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
                    _        -> st

-- ======================================================
-- Sanity probe (known KQK mate-in-1)
-- ======================================================

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
    Nothing -> putStrLn "Solver did not find mate-in-1 here (unexpected)."
    Just pv -> do
      putStrLn ("Solver PV: " ++ unwords (map uciOfMove pv))
      putStrLn "Expected first move among PV: Qc6b7#"

-- ======================================================
-- Random-piece generator
-- ======================================================

-- Public CLI entry:
-- totalPieces >= 2 (two kings included)
-- tries = how many random positions to sample max
genMateRandom :: Int -> Int -> Int -> Bool -> Int -> IO ()
genMateRandom totalPieces mateN want allowAny tries = do
  let extras = totalPieces - 2
  if extras < 0
    then putStrLn "Need at least 2 pieces (the kings)."
    else do
      putStrLn $ "Random mate-in-" ++ show mateN ++ " with " ++ show totalPieces
              ++ " pieces (2 kings + " ++ show extras ++ " random), target "
              ++ show want ++ ", tries " ++ show tries ++ "..."
      loop 0 0
  where
    loop found seen
      | found >= want = putStrLn $ "Done. Reached target " ++ show found ++ "."
      | seen  >= tries = putStrLn $ "Done. Reached tries limit " ++ show tries
                                ++ ". Found " ++ show found ++ "."
      | otherwise = do
          mg <- randomPosition totalPieces
          case mg of
            Nothing -> tick >> loop found (seen+1)
            Just g0 ->
              case matesInExactlyMoves mateN g0 of
                Nothing -> tick >> loop found (seen+1)
                Just pv ->
                  if allowAny || uniqueFirst g0 mateN
                    then do
                      putStrLn "----------------------------------------"
                      putStrLn (prettyBoard (board g0))
                      putStrLn ("White to move. Mate in " ++ show mateN ++ ".")
                      putStrLn ("PV (UCI): " ++ unwords (map uciOfMove pv))
                      tick
                      loop (found+1) (seen+1)
                    else tick >> loop found (seen+1)
      where
        tick = whenTick seen

    whenTick k =
      if k `mod` 1000 == 0 then putStrLn ("sampled " ++ show k ++ " positions") else pure ()

-- Generate a random legal-looking position with:
-- - two kings (not adjacent),
-- - (total-2) random pieces with soft constraints,
-- - White to move.
randomPosition :: Int -> IO (Maybe Game)
randomPosition total = do
  bk <- pickOne allSquares
  let wkChoices = [ s | s <- allSquares, s /= bk, cheb s bk > 1 ]
  if null wkChoices then pure Nothing else do
    wk <- pickOne wkChoices
    let pool0 = delete bk (delete wk allSquares)
    extras <- placeExtras (total - 2) pool0 []
    case extras of
      Nothing     -> pure Nothing
      Just placed -> do
        let bd = place ((wk .= w King) : (bk .= b King) : placed)
        pure . Just $ startGame bd White

-- Try to place k extra random pieces onto distinct squares.
-- Constraints:
--  - avoid pawns on 1st/8th rank
--  - small bias toward White pieces to improve chance of mate (60/40)
placeExtras :: Int -> [Square] -> [(Square, Piece)] -> IO (Maybe [(Square, Piece)])
placeExtras 0 _ acc = pure (Just (reverse acc))
placeExtras k pool acc
  | k < 0     = pure (Just (reverse acc))
  | null pool = pure Nothing
  | otherwise = do
      let pieceTypes = [Queen, Rook, Bishop, Knight, Pawn]
      t  <- pickOne pieceTypes
      wc <- randomRIO (1 :: Int, 10)
      let color = if wc <= 6 then White else Black
      sq <- pickOne pool
      if invalid t color sq
        then placeExtras k pool acc
        else placeExtras (k-1) (delete sq pool) ((sq .= pieceOf color t) : acc)
  where
    pieceOf White pt = w pt
    pieceOf Black pt = b pt
    invalid Pawn White sq = let r = rankOf sq in r == 0 || r == 7
    invalid Pawn Black sq = let r = rankOf sq in r == 0 || r == 7
    invalid _    _     _  = False

-- tiny 'when' without importing Control.Monad
when :: Bool -> IO () -> IO ()
when True  act = act
when False _   = pure ()
