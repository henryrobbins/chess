(** [is_checkmate state] is true iff the player to move in [state] is in
    check and cannot move any pieces. *)
val is_checkmate : Board.t -> bool

(** [is_stalemate state] is true iff the player to move in [state] is
    not in check and cannot move any pieces. *)
val is_stalemate : Board.t -> bool

(** [is_draw state] is true iff players do not have sufficient material
    to deliver a checkmate. *)
val is_draw : Board.t -> bool
