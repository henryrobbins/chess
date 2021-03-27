open Board
open Command

(** This compilation unit contains all functions needed to test the
    validity of a chess move and check the current state (in check?,
    etc.) *)

(** A valid move of the piece at the first square to the second square. *)
type move = square * square

(** This type represents whether or not a player is in [Check]. If they
    are, it carries additional information about the direction in which
    the check is from. [NotCheck] otherwise. *)
type check_state =
  | Check of direction list
  | NotCheck

(** [is_check color board] is the check state of player [color] for the
    given board state [board]. The player is either in check [Check] or
    [NotCheck] *)
val is_check : Board.color -> Board.t -> check_state

(** [valid_piece_moves p b is_check] is the list of all valid moves for
    piece [p] give the current board state [b] with check state
    [is_check]. *)
val valid_piece_moves : Board.p -> Board.t -> check_state -> move list

(** [valid_moves color board] is the list of all valid moves in the
    current board state [board] for player [color]. *)
val valid_moves : Board.color -> Board.t -> move list

(** [is_valid_move move board] is true iff the move [move] is valid for
    the given board state [board]. *)
val is_valid_move : move -> Board.t -> bool
