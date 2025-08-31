## Chess Puzzles 


## Repository Structure

```
app/
  Main.hs               -- CLI entrypoint and flag parsing

src/Chess/
  Types.hs              -- Core data types (Color, PieceType, Piece, Square, Board, Game, etc.)
  Move.hs               -- Move type, UCI parsing/printing helpers
  Board.hs              -- Board representation and ASCII pretty printer
  Setup.hs              -- Helpers to place pieces easily
  Game.hs               -- Game state (board, side to move, castling, ep, clocks), constructors
  MoveGen.hs            -- Pseudo-legal move generation (+ attacksFrom)
  Apply.hs              -- Apply a move with rules (promotion, EP capture effects, castling moves)
  Legality.hs           -- inCheck, legalMoves, checkmate/stalemate; castling path attack checks
  Search.hs             -- Mate-in-N search (at-most/exactly), returning PVs
  Gen.hs                -- Candidate enumerators (used by GenCLI)
  GenCLI.hs             -- Generation UI: focused KQK and random N-piece generators
```

---

## Core Types (Chess.Types)

Key definitions:

* `data Color = White | Black`
* `data PieceType = King | Queen | Rook | Bishop | Knight | Pawn`
* `data Piece = Piece Color PieceType`
* `newtype Square = Square { unSquare :: Int }`

  * Squares are 0..63 with helpers like `at 'e' 4`, `fileOf :: Square -> Int`, `rankOf :: Square -> Int`.
* `data Board = Board {...}` (array or vector–backed; provides `pieceAt :: Board -> Square -> Maybe Piece`, `place :: [(Square, Piece)] -> Board`, `empty :: Board -> Square -> Bool`, etc.)
* `data Castling = Castling { wK :: Bool, wQ :: Bool, bK :: Bool, bQ :: Bool }`
* `data Game = Game
    { board      :: Board
    , toMove     :: Color
    , castling   :: Castling
    , epSquare   :: Maybe Square
    , halfmove   :: Int
    , fullmove   :: Int
    }`

  * Use `startGame :: Board -> Color -> Game` and `exampleGame :: Game` to bootstrap positions.

Helpers for piece literals:

* `w :: PieceType -> Piece` and `b :: PieceType -> Piece`
* Square constructors: `at :: Char -> Int -> Square`, `fileOf`, `rankOf`.

---

## Moves (Chess.Move)

* `data Move = Move { from :: Square, to :: Square, promo :: Maybe PieceType, castle :: Maybe CastleSide }`
* `data CastleSide = KingSide | QueenSide`
* `uciToMove :: String -> Maybe Move` and `uciOfMove :: Move -> String` support I/O.
* Moves to implement promotions use `promo = Just <PieceType>`; castling uses `castle = Just KingSide|QueenSide` with fixed king destination squares.

---

## Board and Setup (Chess.Board, Chess.Setup)

* `prettyBoard :: Board -> String` prints an ASCII diagram with file/rank labels.
* `place :: [(Square, Piece)] -> Board` returns a new board with those pieces set.
* `Setup` exports convenient combinators so you can write:

  ```
  place [ at 'c' 6 .= w Queen
        , at 'c' 7 .= w King
        , at 'a' 8 .= b King
        ]
  ```

  using `(.=) :: Square -> Piece -> (Square, Piece)` defined in modules that need it.

---

## Pseudo-Move Generation (Chess.MoveGen)

* `pseudoMoves :: Game -> [Move]` lists all moves that follow piece movement patterns and basic board occupancy rules but do not filter out self-check.

* `attacksFrom :: Game -> Square -> [Square]` lists attacked squares by the piece on the given square, used by legality checks and king-in-check testing.

* Per-piece generators exist internally (king/queen/rook/bishop/knight/pawn), including:

  * Pawn single/double pushes, captures, promotions, en passant candidates.
  * Castling moves are produced as special `Move` values with `castle = Just ...` but require additional validation in `Legality`.

* A simple `perft :: Int -> Game -> Integer` exists for pseudo-legal node counting.

Note: `perft` here is pseudo-legal; a fully “legal perft” would apply the post-move king safety filter and draw rules.

---

## Applying Moves (Chess.Apply)

`applyMoveRules :: Game -> Move -> Either String Game`

* Validates and applies a single move, updating:

  * Board squares for moves, captures, promotions.
  * En passant capture removal (if applicable).
  * Castling rook movement when the king castles.
  * Castling rights and en passant target square.
  * Halfmove and fullmove clocks.
* Returns `Left <reason>` when the move is disallowed by rules (e.g., illegal promotion type).

`applyMoveRules` does not ensure the *moving* side’s king is not left in check; that filter is handled in `Chess.Legality.legalMoves`.

---

## Legality and Check (Chess.Legality)

Exports:

* `inCheck :: Game -> Color -> Bool` — true if that color’s king square is attacked by the opponent.
* `legalMoves :: Game -> [Move]` — filters `pseudoMoves` by:

  * Forbid “capturing the king” (not a legal chess move).
  * Extra castling validations:

    * Side is not currently in check.
    * No square on the king’s castling path is attacked by the opponent.
    * Destination square matches the castle side’s prescribed square.
  * Apply the move with `applyMoveRules` and ensure the **moving side’s** king is not in check afterward.
* `isCheckmate :: Game -> Bool` and `isStalemate :: Game -> Bool` use the above predicates.

Implementation detail:

* To test “square attacked,” `attacksFrom` is called for all opponent pieces and checked for membership. A temporary `Game` with just the board is sufficient for attack patterns.

---

## Search (Chess.Search)

Mate proof routines:

* `matesInAtMostPlies :: Color -> Int -> Game -> Maybe [Move]`

  * Returns a principal variation (PV) if the root `hero` side can force mate in `<= pl` plies.
  * At `hero` nodes: succeeds if any child succeeds.
  * At opponent nodes: succeeds only if all legal replies still allow a mate proof (a “try-all” universal quantification).
  * Treats stalemate as failure (not mate).
* `matesInAtMostMoves :: Int -> Game -> Maybe [Move]`

  * Converts moves to plies via `2*N - 1` for the side to move at the root, then calls the plies routine.
* `matesInExactlyMoves :: Int -> Game -> Maybe [Move]`

  * Success in `<= N` and failure in `<= N-1`.
* `bestMateLineAtMostMoves :: Int -> Game -> Maybe (Move,[Move])`

  * Convenience to split the PV head and tail.

These routines are correctness-oriented for small `N`, not optimized for speed. They are suitable for endgames and low-branching positions.

---

## Generators (Chess.Gen, Chess.GenCLI)

Two user-facing generators are implemented in `Chess.GenCLI`:

1. **Focused KQK** (`genMateN`)

   * Scans King+Queen vs King endgames where WK is at Chebyshev distance 2 from BK and WQ within 1–3 of BK.
   * Skips terminal positions.
   * For each candidate, calls `matesInExactlyMoves N`.
   * Optional uniqueness filter ensures exactly one winning first move.

2. **Random N-piece** (`genMateRandom`)

   * Places two kings not adjacent, then `(PIECES - 2)` random pieces with basic constraints (no pawns on 1st/8th rank, slight bias toward White pieces to increase mating chances).
   * Samples up to `--tries` positions, checking `matesInExactlyMoves N` for White to move.
   * Optional uniqueness filter.

Helper functions:

* `cheb` for Chebyshev distance.
* `pickOne`, `pickNNoRepeat` using `System.Random.randomRIO`.

---

## CLI (app/Main.hs)

* Parses commands and options. Accepts flags in any order for `gen mate` and `gen random`.
* Commands:

  ```
  simulate
  perft <depth>
  gen mate <N> [COUNT] [--any]
  gen random <PIECES> <N> [COUNT] [--any] [--tries T]
  probe mate1
  ```
* For `simulate`, loops reading UCI strings, applying `applyMoveRules`, and printing board and status using `Chess.Legality`.

---


## Extension Roadmap

1. **FEN I/O**: parser/renderer, plus CLI `solve "<FEN>" --mate N [--unique]`.
2. **Search performance**: alpha-beta with iterative deepening, transposition table (Zobrist hash), killer/history move ordering.
3. **Draw and material rules**: 50-move, threefold repetition, insufficient material.
4. **Puzzle curation**: enforce unique key and “no duals until mate,” export to EPD/JSON (FEN + SAN PV + tags).
5. **Perft-legal**: exact legal perft with golden counts.
6. **More themes**: KRK, KQKP, middlegame motifs with light curation filters.

---

## Build Configuration (cabal)

Make sure the `library` stanza includes all modules and dependencies (notably `random`):

```
library
  exposed-modules:
      Chess.Types
    , Chess.Move
    , Chess.Board
    , Chess.Gen
    , Chess.Setup
    , Chess.Game
    , Chess.Apply
    , Chess.MoveGen
    , Chess.Legality
    , Chess.Search
    , Chess.GenCLI
  hs-source-dirs:     src
  default-language:   Haskell2010
  ghc-options:        -Wall -O2
  build-depends:
      base       >= 4.14 && < 5
    , containers
    , vector
    , random     >= 1.2 && < 2
```

Executable depends on the library:

```cabal
executable chess-puzzles
  main-is:           Main.hs
  hs-source-dirs:    app
  default-language:  Haskell2010
  ghc-options:       -Wall -O2
  build-depends:
      base >= 4.14 && < 5
    , chess-puzzles
```