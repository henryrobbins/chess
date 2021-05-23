(** Maintains the state of a chess board. TODO *)

(** The type of a chess board square identifier in algebraic notation.
    Algebraic notation consists of a lower-case letter followed by a
    number indicating the file (column) and rank (row) of a square
    respectively. *)
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

(** [half_turns t] is the number of elapsed half-turns since the last
    capture or pawn move in state [t]. *)
val half_turns : t -> int

(** [color_of_string s] is the color of the string [s]. Requires: [s] is
    in {w, b, White, Black}. *)
val color_of_string : string -> color

(** [string_of_color c] is the string representing color [c]. *)
val string_of_color : color -> string

(** [piece_id_of_string s] is the piece id of the string [s]. Requires:
    [s] is in {P, R, B, N, Q, K} *)
val piece_id_of_string : string -> piece_type

(** [string_of_piece_id id] is the string representing piece id [id] *)
val string_of_piece_id : piece_type -> string

(** [string_of_piece p] is a string with the color and id of piece [p].
    If the piece is [None], it is a blank space. *)
val string_of_piece : p option -> string

(** [value_of_captured t] is the total value of all captured pieces of
    [color] in game state [t]. *)
val value_of_captured : t -> color -> int

(** [color_to_move t] is the player that should move next in state [t]. *)
val color_to_move : t -> color

(** [can_castle t c p] is true iff the player of color [c] can castle in
    the direction of piece [p] in game state [t]. *)
val can_castle : t -> color -> piece_type -> bool

(** [en_passant_sq t] is a square option which is either the square
    behind a two-square pawn movement made in the previous turn of game
    state [t], or otherwise None. *)
val en_passant_sq : t -> square option

(** [en_passant_sq t] is a piece option which is either the piece
    corresponding to the current en-passant square, or None. *)
val en_passant_piece : t -> p option

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

(** [square_of_king b c] is the square where the king of color [c] is
    located in board state [b] *)
val square_of_king : t -> color -> square

(** [capture_piece t p] is the game state after piece [p] has been
    captured in game state [t]. *)
val capture_piece : t -> p -> t

(** [move_piece t p s turn] is the game state after piece [p] moves to
    square [s] in state [t]. If a piece of the opposite color is already
    at square [s] in state [t], it is captured. Changes the color to
    play iff [turn] is [true]. Requires: [s] is in standard algebraic
    notation. *)
val move_piece : t -> p -> square -> bool -> t

(** [promote_pawn t p id] is the state [t] where the piece [p] is
    promoted to piece type [id]. Requires: [id] is one of Rook, Bishop,
    Knight, or Queen. *)
val promote_pawn : t -> p -> piece_type -> t

(** [is_pawn_promotion t p s] is true if piece [p] is a pawn where
    square [s] is on the other side of the board. *)
val is_pawn_promotion : t -> p -> square -> bool

(** [iterator_from_sq s d] is the list of the squares that can be
    reached by moving in direction [d] from square [s]. Requires: [s] is
    in standard algebraic notation. *)
val iterator_from_sq : square -> direction -> square list

(** [init_from_fen fen] is the board state of the game defined in the
    FEN string [fen]. *)
val init_from_fen : string -> t

(** [export_to_fen t] is the FEN string representing the game state [t]. *)
val export_to_fen : t -> string

(** [init_game] is the game state at the start of a normal chess game. *)
val init_game : unit -> t

(** [board_fen_string t] is the component of the FEN string representing
    the current pieces on the board [t.board]. *)
val board_fen_string : t -> string

(** [print_game_state t] prints the game state [t] in string format. *)
val print_game_state : t -> unit

(** [partition_pieces_by_color lst] is the list of white piece and black
    piece (string representations) respectivley for the pieces in [lst]. *)
val partition_pieces_by_color : p list -> string list * string list
