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
type color =
  | White
  | Black

(** The type of the chess piece. *)
type piece_type =
  | Pawn
  | Rook
  | Bishop
  | Knight
  | Queen
  | King

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

(** The ranks of the chess board. *)
val ranks : string list

(** The files of the chess board. *)
val files : string list

(** [piece_of_square t s] is the piece on square [s] in state [t] if it
    exists. Otherwise, is None. Requires: [s] is in standard algebraic
    notation. *)
val piece_of_square : t -> square -> p option

(** [square_of_piece p] is the square where piece [p] is located.
    Requires: [p] has not been captured. *)
val square_of_piece : p -> square

(** [square_of_king c b] is the square where the king of color [c] is
    located in board state [b] *)
val square_of_king : color -> t -> square

(** [id_of_piece p] is the one-letter string identifier of piece [p]. *)
val id_of_piece : p -> piece_type

(** [color_of_piece p] is the color of piece [p]. *)
val color_of_piece : p -> color

(** [has_moved p] indicates whether piece [p] has been moved. *)
val has_moved : p -> bool

(** [move_piece t p s] is the game state after the piece contained in p
    moves to square s in state [t]. If a piece of the opposite color is
    already at square s in state [t], it is captured. Requires: s is a
    valid destination square. *)
val move_piece : t -> p -> square -> t

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
