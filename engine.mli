open Board

(* [best_move fen] is the best move returned by stockfish in the board
   state represented by the FEN representation [fen].*)
val best_move : string -> (string * string) * piece_type option
