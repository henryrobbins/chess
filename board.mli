(** Maintains the state of a chess board. *)

(** The type of a chess board square identifier in algebraic notation.
    Algebraic notation consists of a lower-case letter followed by a number
    indicating the file (column) and rank (row) of a square respectively. *)
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

(** The type of a direction on the chess board. *)
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

(** The ranks (rows) of the chess board. *)
val ranks : string list

(** The files (columns) of the chess board. *)
val files : string list

(** The abstract type of values representing a chess piece. *)
type p

(** The abstract type of values representing a chess board state. *)
type b

(** The abstract type of values representing the state of a chess game.
    I.e. Current board state, active pieces, and captured pieces. *)
type t

(** [color_to_move t] is the player that should move next in state [t]. *)
val color_to_move : t -> color

(** [active_pieces t] is the list of pieces on the board in state [t]. *)
val active_pieces : t -> p list

(** [captured_pieces t] is the list of captured pieces in state [t]. *)
val captured_pieces : t -> p list

(** [piece_of_square t s] is the piece on square [s] in state [t] if it
    exists. Otherwise, is None. Requires: [s] is in standard algebraic
    notation. *)
val piece_of_square : t -> square -> p option

(** [id_of_piece p] is the piece_type of piece [p]. *)
val id_of_piece : p -> piece_type

(** [color_of_piece p] is the color of piece [p]. *)
val color_of_piece : p -> color

(** [square_of_piece p] is the square where piece [p] is located.
    Requires: [p] has not been captured. *)
val square_of_piece : p -> square

(** [square_of_king c b] is the square where the king of color [c] is
    located in board state [b] *)
val square_of_king : color -> t -> square

(** [has_moved p] indicates whether piece [p] has been moved. *)
val has_moved : p -> bool

(** [capture_piece t p] is the game state after piece [p] has been
    captured in game state [t]. *)
val capture_piece : t -> p -> t

(** [move_piece t p s] is the game state after piece [p] moves to square
    [s] in state [t]. If a piece of the opposite color is already at
    square [s] in state [t], it is captured.
    Requires: [s] is in standard algebraic notation. *)
val move_piece : t -> p -> square -> t

(** [iterator_from_sq s d] is the list of the squares that can be
    reached by moving in direction [d] from square [s].
    Requires: [s] is in standard algebraic notation. *)
val iterator_from_sq : square -> direction -> square list

(** [flip_turn t] is the game state [t], but it is now the opposite
    color's turn to make a move. *)
val flip_turn : t -> t

(** [init_from_json json] is the state of the game in the JSON file [json].
    Requires: [json] is a valid JSON file name representing a board state. *)
val init_from_json : string -> t

(** [init_game] is the game state at the start of a normal chess game. *)
val init_game : unit -> t

(** [print_game_state t] prints the game state [t] in string format. *)
val print_game_state : t -> unit
