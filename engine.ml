open Board

(** [parse_engine_response s] parses the stockfish engine response for a
    move.

    Raises: [Failure "stockfish search failure"] if the engine response
    does not match a known response pattern. *)
let parse_engine_response s =
  let strs = String.split_on_char ' ' s in
  match strs with
  | [ "bestmove"; move; "ponder"; next_move ] -> move
  | [ "bestmove"; move ] -> move
  | _ -> failwith "stockfish search failure"

(** [extract_engine_move s] is the [move] and piece type option (in the
    case of [move] resulting in a pawn promotion) for the move [s]. *)
let extract_engine_move s =
  let sq1 = String.sub s 0 2 in
  let sq2 = String.sub s 2 2 in
  if String.length (String.trim s) = 5 then
    let promote_id =
      String.sub s 4 1 |> String.uppercase_ascii |> piece_id_of_string
    in
    ((sq1, sq2), Some promote_id)
  else ((sq1, sq2), None)

let best_move path fen elo =
  let recover_output channel =
    let rec loop h =
      try loop (input_line channel :: h) with End_of_file -> h
    in
    match loop [] with
    | [] -> failwith "stockfish init failure"
    | h :: t -> h
  in
  let bash_fen = "'" ^ fen ^ "'" in
  let stdout, stdin, stderr =
    Unix.open_process_full
      ("sh ./query_stockfish.sh " ^ path ^ " " ^ bash_fen ^ " " ^ elo)
      [||]
  in
  close_out stdin;
  recover_output stdout |> parse_engine_response |> extract_engine_move
