(** Maintains the state of a puzzle *)
open Board

(** The abstract type of values representing a puzzle. *)
type puz

(** The abstract type of values representing a rush game. *)
type rush

(** The abstract type of values representing an fen. *)
type fen = string

val get_puz_current_board : puz -> fen

val get_player_moves : puz -> fen list

val get_computer_moves : puz -> fen list

val get_wrong : puz -> bool

val get_remaining : rush -> puz list

val current_puz : rush -> puz

val solved_rush : rush -> int

val wrong_rush : rush -> int

(** [puzzle_move puz p m] is the next puzzle step in puzzle [puz], given
    that the user moved piece [p] to square [m]. If [m] was the optimal
    square to move to, [puz] advances to its next state, if there is
    one. If [puz] does not have a next state, [puzzle_step] is true. If
    [m] was not the optimal square, then [puzzle_step] is false. *)
val puzzle_move : puz -> Board.p -> Board.square -> puz

val next_puz_from_rush : rush -> puz -> rush

(** [init_puz_from_fen initial p c] is the puzzle instance created with initial
    board [initial], player moves [p] and computer moves [c]. *)
val init_puz_from_fen : fen -> fen list -> fen list -> puz

(** [play_puzzles] is the current puzzle state, given that we begin in a
    puzzle state. *)
val play_puzzles : rush -> puz
