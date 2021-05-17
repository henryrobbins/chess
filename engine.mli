open Board

(* [best_move fen] is the best move returned by stockfish in the board
   state represented by the FEN representation [fen]. If the move
   requires pawn promotion, the promotion selection is included.
   Otherwise, the second element of the tuple is None. *)
val best_move : string -> (string * string) * piece_type option
