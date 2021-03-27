open Board

(* Command format: [piece identifier] [initial square id] to [final
   square id] What we're checking in this module: 1) [piece identifier]
   is actually on [initial square id] 2) [initial square id] and [final
   square id] are valid board squares

   Ex: "move N a2 to a4" *)

(** This type represents the piece, initial square, and destination
    square for a move on the chess board.*)
type move_phrase = string list

(** This type represents a player command, which can either move a
    piece, quit the game, or [TODO: additional functionality]*)
type command =
  | Move of move_phrase
  | Quit

(** [InvalidSquares] is an exception raised when the player does not
    pass a move command with 2 valid board squares.*)
exception InvalidSquares

(** [InconsistentPlacement] is an exception raised when the player
    passes a command where the given piece is not on the given initial
    square.*)
exception InconsistentPlacement

(** [Malformed] is an exception raised when the player passes a command
    that is either empty or not of the approved format.*)
exception Malformed

(** [parse s] parses a player's string input [s] to a game command. The
    first word becomes a verb, with remaining words defining the
    corresponding game update. Requires: [str] contains only
    alphanumeric (A-Z, a-z, 0-9) and space characters (only ASCII
    character code 32; not tabs or newlines, etc.). Raises:
    [InvalidSquares] if a move string does not contain two distinct,
    valid board squares. Raises: [InconsistentPlacement] if a move
    string passes a piece not lying on the given initial square. Raises:
    [Malformed] if the string does not parse to a command. *)
val parse : string -> Board.t -> command
