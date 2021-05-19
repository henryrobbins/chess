open Board
(** Maintains the state of a puzzle *)

(** The abstract type of values representing a puzzle. *)
type puz
(** [puzzle_move puz p m] is the next puzzle step in puzzle [puz], given
    that the user moved piece [p] to square [m]. If [m] was the optimal
    square to move to, [puz] advances to its next state, if there is
    one. If [puz] does not have a next state, [puzzle_step] is true. If
    [m] was not the optimal square, then [puzzle_step] is false. *)
val puzzle_move: puz -> p -> square -> puz
