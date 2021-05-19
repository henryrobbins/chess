open Board
open Engine

type fen = string
type move = square * piece_type option

type puz = {
    initial_board: fen;

    player_moves: move list;
    computer_moves: fen list;
}

(** [puzzle_history t] is a list of booleans representing which puzzles
    the user has correctly answered. *)
let puzzle_history t = failwith "Unimplemented"

(** [puzzle_streak t] is the number of puzzles the user has answered
    correctly in the puzzle state t. *)
let puzzle_streak t =
  let ph = puzzle_history t in
  let rec helper p acc =
    match p with
    | [] -> acc
    | h :: t -> if h then helper t (acc + 1) else helper t acc
  in
  helper ph 0

(** [failed_count t] is the number of failed puzzles (i.e. the number of
    "strikes") that the player has in state t. *)
let failed_count t = (puzzle_history t |> List.length) - puzzle_streak t

(** [puzzle_move puz p m] is the next puzzle step in puzzle [puz], given
    that the user moved piece [p] to square [m]. If [m] was the optimal
    square to move to, [puz] advances to its next state, if there is
    one. If [puz] does not have a next state, [puzzle_step] is true. If
    [m] was not the optimal square, then [puzzle_step] is false. *)
let rec puzzle_move puz p m = failwith "Unimplemented"

(** [play_puzzles] is the current puzzle state, given that we begin in a
    puzzle state. *)
let rec play_puzzles () = failwith "Unimplemented"

(** [compare_move cur opt] is a boolean representing whether or not the
    current player who's turn it is to move made the best possible move. *)
let compare_move cur opt = failwith "Unimplemented"

(** [init_puzzle p] is the game state initialized from puzzle [p].
    Requires: [p] is an FEN string. *)
let init_puzzle p = init_from_fen p