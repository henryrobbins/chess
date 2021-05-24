(** Maintains the state of a puzzle *)
open Board

type progress =
  | InProgress
  | Complete
  | GameOver
  | Correct
  | Wrong

(** The abstract type of values representing a puzzle. *)
type puzzle

(** The abstract type of values representing a rush game. *)
type rush

(** The abstract type of values representing an fen. *)
type fen = string

(** [current_board rush] is the current board of the puzzle being
    solved. *)
val current_board : rush -> Board.t

(** [total_solved rush] is the number of puzzles the user has answered
    correctly in rush state [rush]. *)
val total_solved : rush -> int

(** [computer_color rush] is the color of the computer in the current
    puzzle. *)
val computer_color : rush -> Board.color

(** [total_wrong rush] is the number of puzzles the user has answered
    incorrectly in rush state [rush]. *)
val total_wrong : rush -> int

(** [update_rush_with_move rush fen] updates the internal representation
    [rush] of the rush depending on the correctness of the given move
    [fen] and whether the puzzle has ended:

    [update_rush_with_move rush fen] is [InProgress] if the current
    puzzle is not finished, [Wrong] if the provided move was incorrect
    but the rush is not over, [Correct] if the provided move was correct
    but the rush is not over, [GameOver] if the rush is over, and
    [Complete] if all puzzles are complete. *)
val update_rush_with_move : rush -> fen -> progress

(** [init_rush] is a randomly generated puzzle list of 20 puzzles. *)
val init_rush : unit -> rush
