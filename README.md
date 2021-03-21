# OCaml Chess

This repo was made as a part of Cornell's CS 3110: Functional Programming
course. Contributors included Nalu Concepcion (lac327), Andy Kim (yk765), Henry
Robbins (hwr26), Anders Wikum (aew236).

## Conventions
- White on south-side of board and black on north-side of board.

## Compilation Units
- board
    - defining the pieces
    - defining the board
    - iterating in directions on the board
    - printing the board
- state
    - two move validation functions (one for in check, one for not)
    - is in check function
- command
    - getting input commands
    - parsing commands
    - calling state functions
    - offer draws
