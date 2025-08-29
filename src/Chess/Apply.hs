module Chess.Apply
  ( applyMoveUnsafe
  ) where

import Chess.Types
import Chess.Board
import Chess.Move
import Chess.Game

-- | Apply a move without full legality checks.
--   - Moves piece (and captures if any)
--   - Handles promotions
--   - Switches side to move
applyMoveUnsafe :: Game -> Move -> Either String Game
applyMoveUnsafe (Game bd side cst _ep hMoves fMoves) mv =
  case pieceAt bd (from mv) of
    Nothing -> Left "No piece on from-square."
    Just p | color p /= side -> Left "Wrong side to move."
    Just p ->
      let capture = case pieceAt bd (to mv) of
                      Nothing -> False
                      Just _  -> True
          p' = case promo mv of
                 Just pt -> Piece side pt
                 Nothing -> p
          bd'   = set (clear bd (from mv)) (to mv) p'
          side' = switchSide side
          h'    = if kind p == Pawn || capture then 0 else hMoves + 1
          f'    = if side' == White then fMoves + 1 else fMoves
      in Right (Game bd' side' cst Nothing h' f')
