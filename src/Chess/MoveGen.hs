module Chess.MoveGen
  ( pseudoMoves
  , pseudoMovesFrom
  , attacksFrom
  , perft
  ) where

import Chess.Types
import Chess.Board
import Chess.Move
import Chess.Game
import Chess.Apply (applyMoveUnsafe)

-- ======================
-- Basic helpers
-- ======================

allSquares :: [Square]
allSquares = [ Square i | i <- [0..63] ]

sqFile :: Square -> Int
sqFile = fileOf

sqRank :: Square -> Int
sqRank = rankOf

mkSquare :: Int -> Int -> Maybe Square
mkSquare f r
  | f >= 0 && f < 8 && r >= 0 && r < 8 = Just (Square (r*8 + f))
  | otherwise                          = Nothing

isFriend :: Board -> Color -> Square -> Bool
isFriend bd side sq = case pieceAt bd sq of
  Just (Piece c _) | c == side -> True
  _                            -> False

isEnemy :: Board -> Color -> Square -> Bool
isEnemy bd side sq = case pieceAt bd sq of
  Just (Piece c _) | c /= side -> True
  _                            -> False

emptySq :: Board -> Square -> Bool
emptySq bd sq = case pieceAt bd sq of
  Nothing -> True
  _       -> False

moveTo :: Square -> Square -> Move
moveTo a b = Move { from = a, to = b, promo = Nothing, castle = Nothing }

promoteMoves :: Square -> Square -> [Move]
promoteMoves a b =
  [ (moveTo a b) { promo = Just Queen  }
  , (moveTo a b) { promo = Just Rook   }
  , (moveTo a b) { promo = Just Bishop }
  , (moveTo a b) { promo = Just Knight }
  ]

-- ======================
-- Public API
-- ======================

-- All pseudo-legal moves for side-to-move (incl. pseudo castling + EP candidates)
pseudoMoves :: Game -> [Move]
pseudoMoves g = concatMap (pseudoMovesFrom g) own ++ castleCandidates g
  where
    bd  = board g
    stm = toMove g
    own = [ sq
          | sq <- allSquares
          , Just (Piece c _) <- [pieceAt bd sq]
          , c == stm
          ]

-- Pseudo-legal moves from a single square
pseudoMovesFrom :: Game -> Square -> [Move]
pseudoMovesFrom g sq = case pieceAt (board g) sq of
  Just (Piece c King)   -> kingMoves   (board g) c sq
  Just (Piece c Queen)  -> queenMoves  (board g) c sq
  Just (Piece c Rook)   -> rookMoves   (board g) c sq
  Just (Piece c Bishop) -> bishopMoves (board g) c sq
  Just (Piece c Knight) -> knightMoves (board g) c sq
  Just (Piece c Pawn)   -> pawnMovesG  g c sq
  _                     -> []

-- Pseudo castling moves (empties + rights only; attack checks handled in Legality)
castleCandidates :: Game -> [Move]
castleCandidates (Game bd side cst _ep _h _f) =
  kside ++ qside
  where
    empties :: [Square] -> Bool
    empties = all (\sq -> pieceAt bd sq == Nothing)

    kside =
      case side of
        White | wK cst && empties [at 'f' 1, at 'g' 1] ->
          [ Move (at 'e' 1) (at 'g' 1) Nothing (Just KingSide) ]
        Black | bK cst && empties [at 'f' 8, at 'g' 8] ->
          [ Move (at 'e' 8) (at 'g' 8) Nothing (Just KingSide) ]
        _ -> []

    qside =
      case side of
        -- require b/c/d files empty so the rook path is clear
        White | wQ cst && empties [at 'b' 1, at 'c' 1, at 'd' 1] ->
          [ Move (at 'e' 1) (at 'c' 1) Nothing (Just QueenSide) ]
        Black | bQ cst && empties [at 'b' 8, at 'c' 8, at 'd' 8] ->
          [ Move (at 'e' 8) (at 'c' 8) Nothing (Just QueenSide) ]
        _ -> []

-- Perft over pseudo-moves (for debugging/bench)
perft :: Int -> Game -> Integer
perft 0 _ = 1
perft d g =
  let ms = pseudoMoves g
  in sum [ perft (d-1) g' | m <- ms, Right g' <- [applyMoveUnsafe g m] ]

-- ======================
-- Piece-specific generators
-- ======================

kingMoves :: Board -> Color -> Square -> [Move]
kingMoves bd side sq =
  [ moveTo sq t
  | (df,dr) <- [ (-1,-1),(0,-1),(1,-1)
               , (-1, 0),        (1, 0)
               , (-1, 1),(0, 1),(1, 1) ]
  , let f = sqFile sq + df
  , let r = sqRank sq + dr
  , Just t <- [mkSquare f r]
  , not (isFriend bd side t)
  ]

knightMoves :: Board -> Color -> Square -> [Move]
knightMoves bd side sq =
  [ moveTo sq t
  | (df,dr) <- [(-2,-1),(-2,1),(2,-1),(2,1),(-1,-2),(-1,2),(1,-2),(1,2)]
  , let f = sqFile sq + df
  , let r = sqRank sq + dr
  , Just t <- [mkSquare f r]
  , not (isFriend bd side t)
  ]

rookMoves :: Board -> Color -> Square -> [Move]
rookMoves = slideMany rookDirs

bishopMoves :: Board -> Color -> Square -> [Move]
bishopMoves = slideMany bishopDirs

queenMoves :: Board -> Color -> Square -> [Move]
queenMoves bd side sq = rookMoves bd side sq ++ bishopMoves bd side sq

slideMany :: [(Int,Int)] -> Board -> Color -> Square -> [Move]
slideMany dirs bd side sq = concatMap (slideOne bd side sq) dirs

slideOne :: Board -> Color -> Square -> (Int,Int) -> [Move]
slideOne bd side sq (df,dr) = go (sqFile sq + df) (sqRank sq + dr) []
  where
    go f r acc = case mkSquare f r of
      Nothing -> acc
      Just t  ->
        if emptySq bd t
          then go (f+df) (r+dr) (acc ++ [moveTo sq t])
        else if isEnemy bd side t
          then acc ++ [moveTo sq t]
          else acc

rookDirs, bishopDirs, queenDirs :: [(Int,Int)]
rookDirs   = [(-1,0),(1,0),(0,-1),(0,1)]
bishopDirs = [(-1,-1),(-1,1),(1,-1),(1,1)]
queenDirs  = rookDirs ++ bishopDirs

-- Pawn moves that also consider the game's current EP target
pawnMovesG :: Game -> Color -> Square -> [Move]
pawnMovesG (Game bd side _cst ep _h _f) c sq =
  normal ++ epCaps
  where
    f0 = sqFile sq
    r0 = sqRank sq
    dir = case c of { White -> 1; Black -> -1 }
    startRank = case c of { White -> 1; Black -> 6 }
    promoRank = case c of { White -> 6; Black -> 1 }  -- rank before promotion move

    step1 = mkSquare f0 (r0 + dir)
    step2 = mkSquare f0 (r0 + 2*dir)

    forward = case step1 of
      Just t | emptySq bd t ->
        if r0 == promoRank then promoteMoves sq t else [moveTo sq t]
      _ -> []

    doubles = case (r0 == startRank, step1, step2) of
      (True, Just s1, Just s2)
        | emptySq bd s1 && emptySq bd s2 -> [moveTo sq s2]
      _ -> []

    cap f r = do
      t <- mkSquare f r
      if isEnemy bd c t
        then if r0 == promoRank then Just (promoteMoves sq t) else Just [moveTo sq t]
        else Nothing

    captures = concat
      [ maybe [] id (cap (f0-1) (r0 + dir))
      , maybe [] id (cap (f0+1) (r0 + dir))
      ]

    normal = forward ++ doubles ++ captures

    -- en-passant candidates (pseudo): capture to ep square if adjacent + plausible pawn behind
    epCaps = case ep of
      Nothing   -> []
      Just epSq ->
        let targetF = sqFile epSq
            targetR = sqRank epSq
            isAdj = abs (targetF - f0) == 1 && (targetR - r0) == dir
            capBehind = mkSquare targetF (targetR - dir)
            validBehind = case capBehind of
              Just s -> case pieceAt bd s of
                          Just (Piece c' Pawn) -> c' /= c
                          _ -> False
              _ -> False
        in if isAdj && validBehind
             then [ moveTo sq epSq ]  -- ApplyRules will remove the captured pawn
             else []

-- ======================
-- Attack maps (used by Legality)
-- ======================

attacksFrom :: Game -> Square -> [Square]
attacksFrom g sq = case pieceAt bd sq of
  Just (Piece c King)   -> kingAttackSquares sq
  Just (Piece _ Queen)  -> sliderAttackSquares bd sq queenDirs
  Just (Piece _ Rook)   -> sliderAttackSquares bd sq rookDirs
  Just (Piece _ Bishop) -> sliderAttackSquares bd sq bishopDirs
  Just (Piece _ Knight) -> knightAttackSquares sq
  Just (Piece c Pawn)   -> pawnAttackSquares c sq
  _                     -> []
  where
    bd = board g

kingAttackSquares :: Square -> [Square]
kingAttackSquares sq =
  [ t
  | (df,dr) <- [ (-1,-1),(0,-1),(1,-1)
               , (-1, 0),        (1, 0)
               , (-1, 1),(0, 1),(1, 1) ]
  , let f = sqFile sq + df
  , let r = sqRank sq + dr
  , Just t <- [mkSquare f r]
  ]

knightAttackSquares :: Square -> [Square]
knightAttackSquares sq =
  [ t
  | (df,dr) <- [(-2,-1),(-2,1),(2,-1),(2,1),(-1,-2),(-1,2),(1,-2),(1,2)]
  , let f = sqFile sq + df
  , let r = sqRank sq + dr
  , Just t <- [mkSquare f r]
  ]

pawnAttackSquares :: Color -> Square -> [Square]
pawnAttackSquares side sq =
  [ t
  | df <- [-1,1]
  , let dir = case side of { White -> 1; Black -> -1 }
  , let f = sqFile sq + df
  , let r = sqRank sq + dir
  , Just t <- [mkSquare f r]
  ]

sliderAttackSquares :: Board -> Square -> [(Int,Int)] -> [Square]
sliderAttackSquares bd sq dirs = concatMap go dirs
  where
    go (df,dr) = ray (sqFile sq + df) (sqRank sq + dr)
      where
        ray f r = case mkSquare f r of
          Nothing -> []
          Just t  -> t : case pieceAt bd t of
                           Nothing -> ray (f+df) (r+dr)
                           Just _  -> []
