(** Provides functions for checking the endgame state of a chess board. *)

(** [is_checkmate state] is true iff the player to move in [state] is in
    check and cannot move any pieces. *)
val is_checkmate : Board.t -> bool

(** [is_stalemate state] is true iff the player to move in [state] is
    not in check and cannot move any pieces. *)
val is_stalemate : Board.t -> bool

(** [is_draw state] is true iff players do not have sufficient material
    to deliver a checkmate or it has been 50 half-turns since the last
    capture or pawn move. *)
val is_draw : Board.t -> bool
