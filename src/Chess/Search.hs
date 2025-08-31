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

-- | Try to prove that 'hero' (the side to move at root) can force mate
--   in at most 'plies' plies. Returns a PV (sequence of moves, both sides) if successful.
matesInAtMostPlies :: Color -> Int -> Game -> Maybe [Move]
matesInAtMostPlies hero pl g
  | isCheckmate g = if toMove g /= hero then Just [] else Nothing
  | pl <= 0       = Nothing
  | otherwise     =
      if toMove g == hero
        then tryAny (legalMoves g)   $ \m -> stepHero m
        else tryAll (legalMoves g)   $ \m -> stepOpp  m
  where
    stepHero m = do
      g' <- applyOK g m
      (m:) <$> matesInAtMostPlies hero (pl-1) g'
    stepOpp m = do
      g' <- applyOK g m
      -- opponent chooses the toughest line; for PV we can pick any (e.g. first)
      pv  <- matesInAtMostPlies hero (pl-1) g'
      pure (m:pv)

    -- Succeeds if ANY child succeeds
    tryAny [] _ = Nothing
    tryAny (m:ms) f = case f m of
      Just pv -> Just pv
      Nothing -> tryAny ms f

    -- Succeeds only if ALL children succeed; picks one PV to return
    tryAll [] _ = Just []   -- vacuously true (no replies)
    tryAll ms f = foldr go (Just []) ms
      where
        go _  Nothing  = Nothing
        go m (Just _)  = case f m of
                           Nothing -> Nothing
                           Just pv -> Just pv

    applyOK st m = case applyMoveRules st m of
                     Right g' -> Just g'
                     _        -> Nothing

-- | Interpret N as "N moves for side-to-move" (so plies = 2*N - 1).
matesInAtMostMoves :: Int -> Game -> Maybe [Move]
matesInAtMostMoves n g = matesInAtMostPlies (toMove g) (max 0 (2*n - 1)) g

-- | Exactly N moves = at most N AND not at most (N-1).
matesInExactlyMoves :: Int -> Game -> Maybe [Move]
matesInExactlyMoves n g =
  case matesInAtMostMoves n g of
    Nothing -> Nothing
    yesPV   -> if n <= 1 then yesPV
               else case matesInAtMostMoves (n-1) g of
                      Nothing -> yesPV
                      Just _  -> Nothing

-- | Convenience: also return the first move (useful for uniqueness checks).
bestMateLineAtMostMoves :: Int -> Game -> Maybe (Move, [Move])
bestMateLineAtMostMoves n g = do
  pv <- matesInAtMostMoves n g
  case pv of
    (m:rest) -> Just (m, rest)
    _        -> Nothing
