open Board
(** [puzzle_history t] is a list of booleans representing which puzzles
    the user has correctly answered. *)
val puzzle_history: t -> bool list

(** [puzzle_streak t] is the number of puzzles the user has answered
    correctly in the puzzle state t. *)
val puzzle_streak: t -> int

(** [failed_count t] is the number of failed puzzles (i.e. the number of
    "strikes") that the player has in state t. *)
val failed_count: t -> int

(** [puzzle_move puz p m] is the next puzzle step in puzzle [puz], given
    that the user moved piece [p] to square [m]. If [m] was the optimal
    square to move to, [puz] advances to its next state, if there is
    one. If [puz] does not have a next state, [puzzle_step] is true. If
    [m] was not the optimal square, then [puzzle_step] is false. *)
val puzzle_move: t -> p -> square

(** [play_puzzles] is the current puzzle state, given that we begin in a
    puzzle state. *)
val play_puzzles: unit