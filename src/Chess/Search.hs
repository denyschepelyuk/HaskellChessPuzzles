module Chess.Search
  ( matesInAtMostPlies
  , matesInAtMostMoves
  , matesInExactlyMoves
  , bestMateLineAtMostMoves
  ) where

import Chess.Types
import Chess.Game
import Chess.Move
import Chess.Legality (legalMoves, isCheckmate, isStalemate)
import Chess.Apply    (applyMoveRules)

-- Prove that 'hero' (side at root) can force mate in <= pl plies.
-- Returns a PV (sequence of moves, both sides) on success.
matesInAtMostPlies :: Color -> Int -> Game -> Maybe [Move]
matesInAtMostPlies hero pl g
  | isCheckmate g = if toMove g /= hero then Just [] else Nothing
  | isStalemate g = Nothing
  | pl <= 0       = Nothing
  | otherwise     =
      if toMove g == hero
        then tryAny (legalMoves g)   stepHero
        else tryAll (legalMoves g)   stepOpp
  where
    stepHero m = do
      g' <- applyOK g m
      (m:) <$> matesInAtMostPlies hero (pl-1) g'
    stepOpp m = do
      g' <- applyOK g m
      pv <- matesInAtMostPlies hero (pl-1) g'
      pure (m:pv)

    -- succeeds if ANY branch succeeds
    tryAny [] _ = Nothing
    tryAny (m:ms) f = case f m of
      Just pv -> Just pv
      Nothing -> tryAny ms f

    -- succeeds only if ALL branches succeed; returns one PV
    tryAll [] _ = Just []   -- no replies
    tryAll (m:ms) f = case f m of
      Nothing -> Nothing
      Just pv -> case tryAll ms f of
                   Nothing -> Nothing
                   Just _  -> Just pv

    applyOK st m = case applyMoveRules st m of
                     Right g' -> Just g'
                     _        -> Nothing

-- Interpret N as "N moves for side-to-move" → plies = 2*N - 1
matesInAtMostMoves :: Int -> Game -> Maybe [Move]
matesInAtMostMoves n g = matesInAtMostPlies (toMove g) (max 0 (2*n - 1)) g

-- Exactly N = at most N AND not at most (N-1)
matesInExactlyMoves :: Int -> Game -> Maybe [Move]
matesInExactlyMoves n g =
  case matesInAtMostMoves n g of
    Nothing -> Nothing
    yesPV   ->
      if n <= 1 then yesPV
      else case matesInAtMostMoves (n-1) g of
             Nothing -> yesPV
             Just _  -> Nothing

-- Convenience: return first move and rest of PV
bestMateLineAtMostMoves :: Int -> Game -> Maybe (Move, [Move])
bestMateLineAtMostMoves n g = do
  pv <- matesInAtMostMoves n g
  case pv of
    (m:rest) -> Just (m, rest)
    _        -> Nothing
