# Turing

`Turing` is a simple solver for mathematical puzzles. The program is
able to solve simple instances of the following games:

* [Garam](https://www.garam.fr/garam/garam_en_ligne/tutoriel/): fill a
  grid so that each operation is correct.

* [Futoshiki](https://en.wikipedia.org/wiki/Futoshiki): fill a 6x6
  grid with digits so that each column and each row contains all the
  digits from 1 to 6, and all order relationships given in the grid
  are enforced.

* [Sudoku](https://en.wikipedia.org/wiki/Sudoku): fill a 9x9 grid with
  digits so that each column, each row, and each of the nine 3x3
  blocks that compose the grid contains all the digits from 1 to 9.

`Turing` is also able to generate Garam puzzles.

## TODO

- [ ] Detect broken grids for Sudoku and Futoshiki.

- [ ] Refactor and reorder the code to try to decouple the different
      puzzle types.

- [ ] Add generators for Sudoku and Futoshiki.

- [ ] Decouple the resolution/generation and the input/output. This
      will allow to switch more easily between file formats.
