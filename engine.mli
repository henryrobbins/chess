(** Provides a function for interacting with the
   {{: https://stockfishchess.org/} Stockfish 13} open source chess engine. *)

open Board

(** [best_move fen elo] is the move returned by stockfish with estimated
    strength [elo] in the board state represented by the FEN
    representation [fen]. If the move requires pawn promotion, the
    promotion selection is included. Otherwise, the second element of the
    tuple is None. *)
val best_move :
  string -> string -> (string * string) * piece_type option
