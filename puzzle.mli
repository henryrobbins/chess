(** Maintains the state of a puzzle *)
open Board

(** The abstract type of values representing a puzzle. *)
type puz

(** The abstract type of values representing a rush game. *)
type rush

(** The abstract type of values representing an fen. *)
type fen = string

(** [get_puz_current_board puz] is the current board in puzzle state [puz].*)
val get_puz_current_board : puz -> fen

(** [get_player_moves puz] is the list of FENs of optimal moves in puzzle
    state [puz].*)
val get_player_moves : puz -> fen list

(** [get_computer_moves puz] is the list of FENs of the computer in response to
    the optimal moves in puzzle state [puz]. *)
val get_computer_moves : puz -> fen list

(** [get_wrong puz] represents whether or not the player has incorrectly
    answered the puzzle [puz]. *)
val get_wrong : puz -> bool

(** [get_remaining rush] is a list of remaining puzzles the player has to solve
    in rush state [rush].*)
val get_remaining : rush -> puz list

(** [current_puz rush] is the current puzzle being solved in rush state [rush].
    *)
val current_puz : rush -> puz

(** [solved_rush rush] is the number of puzzles the user has answered correctly
in rush state [rush]. *)
val solved_rush : rush -> int

(** [wrong_rush rush] is the number of puzzles the user has answered
    incorrectly in rush state [rush]. *)
val wrong_rush : rush -> int

(** [puzzle_move puz fen] is the next puzzle step in puzzle [puz], given that
    the user's move resulted in fen [fen]. *)
val puzzle_move : puz -> fen -> puz

(** [make_rush puz_list init] is the rush instance created by beginning in
    puzzle [init] and continuing through [puz_list].*)
val make_rush : puz list -> puz -> rush

(** [get_next_puzzle rush] is the next puzzle from the remaining puzzles of
    [rush]. *)
val get_next_puzzle : rush -> puz

(** [init_rush] is a randomly generated puzzle list of 20 puzzles. *)
val init_rush : unit -> puz list

(** [next_puz_from_rush rush] returns the next puzzle from the remaining puzzles
    of rush state [rush].*)
val next_puz_from_rush : rush -> rush

(** [init_puz_from_fen initial p c] is the puzzle instance created with initial
    board [initial], player moves [p] and computer moves [c]. *)
val init_puz_from_fen : fen -> fen list -> fen list -> puz

(** [play_puzzles rush] is the current puzzle state, given that we begin in a
    rush state [rush]. *)
val play_puzzles : rush -> puz
