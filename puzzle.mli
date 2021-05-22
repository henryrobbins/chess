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

(** [puzzle_move puz fen] is the next puzzle step in puzzle [puz], given that 
    the user's move resulted in fen [fen]. *)
val puzzle_move : puz -> fen -> puz

(** [init_rush rush] is the puzzle state beginning from rush state [rush]. *)
val init_rush : rush -> rush 

val next_puz_from_rush : rush -> puz -> rush

(** [make_rush puz_list init] is the rush instance created by beginning in 
    puzzle [init] and continuing through [puz_list].*)
val make_rush : puz list -> puz -> rush

(** [init_puz_from_fen initial p c] is the puzzle instance created with initial
    board [initial], player moves [p] and computer moves [c]. *)
val init_puz_from_fen : fen -> fen list -> fen list -> puz

(** [play_puzzles rush] is the current puzzle state, given that we begin in a
    rush state [rush]. *)
val play_puzzles : rush -> puz
