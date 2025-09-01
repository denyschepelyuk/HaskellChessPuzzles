# Chess Puzzles

This project generates and verifies chess “mate in X moves” puzzles. It includes a CLI to simulate positions, count nodes (perft), generate simple endgame mates, and sample random positions for mate-in-X discoveries.

## Running

```
cabal update
cabal build
````

---

## Quick Start

### Show usage banner and a demo board

```
cabal run chess-puzzles
```

### Simulate moves interactively

```
cabal run chess-puzzles -- simulate
```

* Enter UCI moves like `e2e4`, `g7g8Q`.
* Type `q` or `quit` to exit.
* After each move, the program prints the board and a short status line:

  * `Check!`, `Checkmate.`, or `Stalemate.`

### Count nodes (perft) from a demo position

```
cabal run chess-puzzles -- perft 2
```

Counts pseudo-legal move tree size to the given depth.

### Generate focused KQK mates

```
# find 5 positions where White mates in 1
cabal run chess-puzzles -- gen mate 1

# find 10 positions where White mates in 2
cabal run chess-puzzles -- gen mate 2 10

# allow multiple winning first moves (faster to find examples)
cabal run chess-puzzles -- gen mate 2 10 --any
```

This scans King+Queen vs King positions around realistic king/queen distances and prints puzzles that are mate in exactly **N** moves for White to move.

### Sanity probe (known mate-in-1)

```
cabal run chess-puzzles -- probe mate1
```

Prints a known KQK position where `Qc6b7#` exists and shows the solver’s principal variation (PV).

### Generate random N-piece positions and search for mate-in-X

```
# 5 pieces total (two kings + 3 random), find 5 puzzles that are mate in 2,
# allow any winning key move, try up to 50,000 random positions
cabal run chess-puzzles -- gen random 5 2 5 --any --tries 50000
```

**Parameters:**

* `PIECES`: total pieces on board including both kings (minimum 2).
* `N`: required mate distance in moves for side to move (White).
* `COUNT` (optional, default 5): how many puzzles to print before stopping.
* `--any`: do not enforce a unique winning first move (faster).
* `--tries T`: limit the number of random samples to `T` before stopping.

---

## Output Format

For each found puzzle:

* ASCII board with files/ranks.
* Side to move and the mate distance (e.g. “White to move. Mate in 2.”).
* PV in UCI moves, e.g. `PV (UCI): c6b7 a8a7 b7a7#`

At the end of generation scans, a summary line reports how many candidates were scanned and how many puzzles were found.


---

## Command Reference

```
simulate
perft <depth>
gen mate <N> [COUNT] [--any]
gen random <PIECES> <N> [COUNT] [--any] [--tries T]
probe mate1
```
