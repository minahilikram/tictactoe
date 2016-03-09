# tictactoe-FORTRAN

[Tictactoe](https://en.wikipedia.org/wiki/Sudoku) an implementation using FORTRAN Algorithms.

> Tic-tac-toe, a game where a player (X) plays against a computer (O) using a very simple AI engine which involves checking the eight paths through the board and deriving a sum for each path based on a value for each of X (1), O (4), or blank (0). The algorithm first searches for a sum of 8, which signifies that there are two O’s in a path, and then proceeds to add the third O in order to win the game. If this scenario does not work, the algorithm then looks for a sum of 2, signifying a path with two X’s and a potential win for the opponent. If one exists, a O is used to block that path. In neither of those situations exist, the algorithm chooses a random spot to place an O.

## Install

```sh
$ git clone https://github.com/minahilikram/tictactoe-FORTRAN
```

## Usage

```sh
$ cd tictactoe-FORTRAN/
$ make
$ make run
```

## Limitations

On user input prompt, a number between 1 and 9 must be entered; pressing simply "enter" doesn't prompt user of any indication that the input is incorrect. Any other input is tested, user is prompted.
