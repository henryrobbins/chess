(** Maintains the state of a puzzle *)
open Board

type progress = InProgress | Complete | GameOver | Correct | Wrong

(** The abstract type of values representing a puzzle. *)
type puzzle

(** The abstract type of values representing a rush game. *)
type rush

(** The abstract type of values representing an fen. *)
type fen = string

(** [current_puz rush] is the current] board of the puzzle being solved. *)
val current_board : rush -> Board.t

(** [solved_rush rush] is the number of puzzles the user has answered correctly
in rush state [rush]. *)
val total_solved : rush -> int

(** [computer_color rush] is the color of the computer in the current puzzle. *)
val computer_color : rush -> Board.color

(** [total_wrong rush] is the number of puzzles the user has answered
    incorrectly in rush state [rush]. *)
val total_wrong : rush -> int

(** [update_rush_with_move rush fen] TODO *)
val update_rush_with_move : rush -> fen -> progress

(** [init_rush] is a randomly generated puzzle list of 20 puzzles. *)
val init_rush : unit -> rush

