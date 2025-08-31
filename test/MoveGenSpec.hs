module Main (main) where

import Test.Hspec
import Chess.Types
import Chess.Board
import Chess.Game
import Chess.MoveGen (pseudoMoves, pseudoMovesFrom)
import Chess.Move (uciOfMove)  -- 👈 add this

infix 1 .=
(.=) :: Square -> Piece -> (Square, Piece)
(.=) = (,)

main :: IO ()
main = hspec $ do
  describe "knight mobility" $ do
    it "d4 has 8 knight moves on empty board" $ do
      let bd = empty
          g  = startGame bd White
          sq = at 'd' 4
          g' = g { board = set bd sq (w Knight) }
      length (pseudoMovesFrom g' sq) `shouldBe` 8

    it "a1 has 2 knight moves on empty board" $ do
      let bd = empty
          g  = startGame bd White
          sq = at 'a' 1
          g' = g { board = set bd sq (w Knight) }
      length (pseudoMovesFrom g' sq) `shouldBe` 2

  describe "pawn double push" $ do
    it "white pawn on a2 can double if clear" $ do
      let bd0 = empty
          bd1 = set bd0 (at 'a' 2) (w Pawn)
          g   = startGame bd1 White
          ms  = pseudoMovesFrom g (at 'a' 2)
      any (\m -> uciOfMove m == "a2a4") ms `shouldBe` True   -- 👈 changed

    it "blocked a3 prevents double push" $ do
      let bd0 = empty
          bd1 = set bd0 (at 'a' 2) (w Pawn)
          bd2 = set bd1 (at 'a' 3) (b Pawn)
          g   = startGame bd2 White
          ms  = pseudoMovesFrom g (at 'a' 2)
      any (\m -> uciOfMove m == "a2a4") ms `shouldBe` False  -- 👈 changed

  describe "perft surface check" $ do
    it "depth-1 count matches pseudoMoves length" $ do
      let bd = empty
          bd' = set bd (at 'e' 4) (w Queen)
          g   = startGame bd' White
      length (pseudoMoves g) `shouldSatisfy` (>= 1)
