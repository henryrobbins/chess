(** Provides functions for validating chess moves. *)

open Board
open Command

(** This compilation unit contains all functions needed to test the
    validity of a chess move in a provided game state. *)

(** A valid move of the piece at the first square to the second square. *)
type move = square * square

(** This type represents whether or not a player is in [Check]. If they
    are, it carries additional information about the direction in which
    the check is from. [NotCheck] otherwise. *)
type check_state =
  | Check of direction list
  | NotCheck

(** [get_checks state] is either [Check dir] where [dir] is a list of
    directions from which the player-to-move is in check in [state], or
    [NotCheck] if there is no such direction. *)
val get_checks : Board.t -> check_state

(** [valid_piece_moves state piece] is the list of all valid moves for
    [piece] in the current game state [state]. *)
val valid_piece_moves : Board.t -> Board.p -> move list

(** [valid_moves state] is the list of all valid moves in the current
    board state [state] for the player-to-move. *)
val valid_moves : Board.t -> move list

(** [is_valid_move move state] is true iff [move] is a valid move for
    the player-to-move in game state [state]. *)
val is_valid_move : move -> Board.t -> bool
