Exploring Ersatz
================

This package explores encoding and solving various puzzles using
[Ersatz](https://hackage.haskell.org/package/ersatz). Ersatz is
a library for generating
[SAT problems](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
with a high-level level encoding.

In particular this library explores the use of a new symbolic set selection
encoding named `Select` and the use of a total map type named `SparseMap`.

Select makes it easy to work symbolically with a choice from a set of
possibilities without creating a new encoding of the enumerated type
in Ersatz. This is particularly useful for run-time defined possibilities.

SparseMap supports the generalized Boolean typeclass and makes it easy
to work with bit-maps (or more complex types than bit) over arbitrary keys.

Problems encoded
----------------

* **Cube** - Solves a puzzle where 17 rectangular solids are placed within
  a 5x5 cube.

* **Pentomino** - Solves placing arbitrary puzzle pieces on an arbitrary 2D
    board allowing for rotating and flipping pieces. The example problem
    places pentominos on a square board.

* **Domino** - Solve the covering of an NxM grid with dominos where two
    opposing corners are removed

* **Einstein** - Solves a classic logic puzzle.

* **RegExp** - Solves regular expression crossword puzzles. This
   implementation features both an efficient implementation of regular
   expressions without backreferences, and an inefficient one with
   backreferences.

* **Nonogram** - Solves [nonogram](https://en.wikipedia.org/wiki/Nonogram)
   puzzles by encoding the clues as regular expressions and treating them
   like regular expression crossword puzzles.

* **Sudoku** - Solves arbitrary [Sudoku](https://en.wikipedia.org/wiki/Sudoku) puzzles.

* **Tough** - Solves the [One Tough Puzzle](http://www.alexbrands.com/product/games/one-tough-puzzle/)
   3x3 jigsaw puzzle.

* **SetGame** - Computes a way to arrange all 81 [Set](https://en.wikipedia.org/wiki/Set_(game))
   cards into 27 sets.

* **Queens** - Solves the generalized [Eight queens puzzle](https://en.wikipedia.org/wiki/Eight_queens_puzzle)
