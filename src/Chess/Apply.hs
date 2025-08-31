module Chess.Apply
  ( applyMoveUnsafe   -- keep for perft over pseudo-moves
  , applyMoveRules    -- rule-aware apply: EP, castling, flags
  ) where

import Chess.Types
import Chess.Board
import Chess.Move
import Chess.Game

-- Unsafe move (already in your project)
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

-- === Rule-aware apply ===
applyMoveRules :: Game -> Move -> Either String Game
applyMoveRules (Game bd side cst ep hMoves fMoves) mv =
  case pieceAt bd (from mv) of
    Nothing -> Left "No piece on from-square."
    Just p | color p /= side -> Left "Wrong side to move."
    Just p ->
      case (kind p, castle mv) of
        (King, Just cs) -> do
          -- Castling
          (kFrom, kTo, rFrom, rTo) <- castleSquares side cs
          if from mv /= kFrom || to mv /= kTo
            then Left "Bad castling squares."
            else do
              -- empties were checked in pseudo gen; re-check here for safety
              let bd1 = clear bd kFrom
                  bd2 = clear bd1 rFrom
                  bd3 = set bd2 rTo (Piece side Rook)
                  bd4 = set bd3 kTo (Piece side King)
                  side' = switchSide side
                  cst'  = disableCastling side cst
                  h'    = hMoves + 1
                  f'    = if side' == White then fMoves + 1 else fMoves
              pure (Game bd4 side' cst' Nothing h' f')

        (Pawn, _) -> do
          -- En-passant or normal pawn move
          let fromF = fileOf (from mv); fromR = rankOf (from mv)
              toF   = fileOf (to mv)  ; toR   = rankOf (to mv)
              dir   = if side == White then 1 else -1
              isDiag = abs (toF - fromF) == 1 && (toR - fromR) == dir
              isDouble = (toF == fromF) && (toR - fromR) == 2 * dir
              promotionPiece = case promo mv of
                                  Just pt -> pt
                                  Nothing -> Pawn
          if isDiag && pieceAt bd (to mv) == Nothing
            then -- EP capture
              case ep of
                Just epSq | epSq == to mv -> do
                  let capR = toR - dir
                      capSq = Square (capR * 8 + toF)
                  case pieceAt bd capSq of
                    Just (Piece opp Pawn) | opp /= side -> do
                      let bd1 = clear bd (from mv)
                          bd2 = clear bd1 capSq
                          bd3 = set bd2 (to mv) (Piece side promotionPiece)
                          side' = switchSide side
                          h' = 0
                          f' = if side' == White then fMoves + 1 else fMoves
                          cst' = updateCastlingOnMove side (from mv) (to mv)
                                $ updateCastlingOnCapture (switchSide side) capSq cst
                      pure (Game bd3 side' cst' Nothing h' f')
                    _ -> Left "Invalid en-passant capture."
                _ -> Left "Diagonal pawn move to empty square is invalid."
            else do
              -- Normal pawn move / capture / promotion
              let capture = case pieceAt bd (to mv) of { Nothing -> False; _ -> True }
                  p' = Piece side (case promo mv of { Just pt -> pt; Nothing -> Pawn })
                  bd1 = set (clear bd (from mv)) (to mv) p'
                  ep' = if isDouble then mkSkipped fromF fromR dir else Nothing
                  side' = switchSide side
                  h' = 0
                  f' = if side' == White then fMoves + 1 else fMoves
                  cst' = updateCastlingOnMove side (from mv) (to mv)
                       $ updateCastlingOnCapture (switchSide side) (to mv) cst
              pure (Game bd1 side' cst' ep' h' f')

        _ -> do
          -- Any other piece
          let capture = case pieceAt bd (to mv) of { Nothing -> False; _ -> True }
              p' = case promo mv of { Just pt -> Piece side pt; Nothing -> p }
              bd1 = set (clear bd (from mv)) (to mv) p'
              side' = switchSide side
              h' = if capture then 0 else hMoves + 1
              f' = if side' == White then fMoves + 1 else fMoves
              -- update castling rights if king/rook move or a rook is captured
              cst1 = updateCastlingOnMove side (from mv) (to mv) cst
              cst' = updateCastlingOnCapture (switchSide side) (to mv) cst1
          pure (Game bd1 side' cst' Nothing h' f')

-- Helpers

castleSquares :: Color -> CastleSide -> Either String (Square, Square, Square, Square)
castleSquares White KingSide  = Right (at 'e' 1, at 'g' 1, at 'h' 1, at 'f' 1)
castleSquares White QueenSide = Right (at 'e' 1, at 'c' 1, at 'a' 1, at 'd' 1)
castleSquares Black KingSide  = Right (at 'e' 8, at 'g' 8, at 'h' 8, at 'f' 8)
castleSquares Black QueenSide = Right (at 'e' 8, at 'c' 8, at 'a' 8, at 'd' 8)

disableCastling :: Color -> Castling -> Castling
disableCastling White c = c { wK = False, wQ = False }
disableCastling Black c = c { bK = False, bQ = False }

mkSkipped :: Int -> Int -> Int -> Maybe Square
mkSkipped f r dir =
  let r' = r + dir
  in if r' >= 0 && r' < 8 then Just (Square (r' * 8 + f)) else Nothing

-- If a king moves, clear that side's castling. If a rook moves from its home, clear that side's flag.
updateCastlingOnMove :: Color -> Square -> Square -> Castling -> Castling
updateCastlingOnMove side fromSq _ cst =
  case side of
    White ->
      case pieceAtHome White fromSq of
        HomeKing     -> cst { wK = False, wQ = False }
        HomeRookK    -> cst { wK = False }
        HomeRookQ    -> cst { wQ = False }
        HomeOther    -> cst
    Black ->
      case pieceAtHome Black fromSq of
        HomeKing     -> cst { bK = False, bQ = False }
        HomeRookK    -> cst { bK = False }
        HomeRookQ    -> cst { bQ = False }
        HomeOther    -> cst

-- If a rook on its home square is captured, clear that side's right.
updateCastlingOnCapture :: Color -> Square -> Castling -> Castling
updateCastlingOnCapture side toSq cst =
  case (side, toSq) of
    (White, sq) | sq == at 'h' 8 -> cst { bK = False }
    (White, sq) | sq == at 'a' 8 -> cst { bQ = False }
    (Black, sq) | sq == at 'h' 1 -> cst { wK = False }
    (Black, sq) | sq == at 'a' 1 -> cst { wQ = False }
    _ -> cst

data HomeSquare = HomeKing | HomeRookK | HomeRookQ | HomeOther

pieceAtHome :: Color -> Square -> HomeSquare
pieceAtHome White sq
  | sq == at 'e' 1 = HomeKing
  | sq == at 'h' 1 = HomeRookK
  | sq == at 'a' 1 = HomeRookQ
  | otherwise      = HomeOther
pieceAtHome Black sq
  | sq == at 'e' 8 = HomeKing
  | sq == at 'h' 8 = HomeRookK
  | sq == at 'a' 8 = HomeRookQ
  | otherwise      = HomeOther
