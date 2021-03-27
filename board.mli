(** The abstract type of values representing a chess piece. *)
type p

(** The abstract type of values representing a chess board state. *)
type b

(** The abstract type of values representing the state of a chess game.
    I.e. Current board state, active pieces, and captured pieces. *)
type t

(* TODO: Define algebraic notation. *)

(** The type of a chess board square identifier in algebraic notation. *)
type square = string

(** The color of the player. *)
type color = White | Black

(** The type of the chess piece. *)
type piece_type = Pawn | Rook | Bishop | Knight | Queen | King

(** The type of a direction. *)
type direction =
  | N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  | L

(** [piece_of_square t s] is the piece on square [s] in state [t] if it
    exists. Otherwise, is None. Requires: [s] is in standard algebraic
    notation. *)
val piece_of_square : t -> square -> p option

(** [square_of_piece p] is the square where piece [p] is located if [p]
    has not been captured. Otherwise, is None. *)
val square_of_piece : p option -> square option

(** [id_of_piece p] is the one-letter string identifier of piece [p]. *)
val id_of_piece : p option -> piece_type

(** [move_piece t p s] is the game state after the piece contained in p
    moves to square s in state [t]. If a piece of the opposite color is
    already at square s in state [t], it is captured. Requires: p is a
    non-None piece option, s is a valid destination square. *)
val move_piece : t -> p option -> square -> t

(** [color_of_piece p] is the color of piece [p]. *)
val color_of_piece : p option -> color

(** [iterator_from_sq s d] is the list of the squares that can be
    reached by moving in direction [d] from square [s]. Requires: [s] is
    in standard algebraic notation. *)
val iterator_from_sq : square -> direction -> square list

(** [active_pieces t] is the list of pieces on the board in state [t]. *)
val active_pieces : t -> p list

(** [captured_pieces t] is the list of captured pieces in state [t]. *)
val captured_pieces : t -> p list

(* TODO: Use Yojson.Basic.t here. Need to figure out how to do this.. *)

(** [init_from_json json] is the state of the game read in from the JSON
    file [json]. Requires: [json] is a valid JSON file representing a
    board state. *)
val init_from_json : Yojson.Basic.t -> t

(** [init_game] is the game state at the start of a normal chess game.*)
val init_game : unit -> t

(** [print_game_state t] prints the game state [t] in string format. *)
val print_game_state : t -> unit
