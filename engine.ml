open Board

let parse_engine_response s =
  print_endline s;
  let strs = String.split_on_char ' ' s in
  match strs with
  | [ "bestmove"; move; "ponder"; next_move ] -> move
  | [ "bestmove"; move ] -> move
  | _ -> failwith "stockfish search failure"

let extract_engine_move s =
  let sq1 = String.sub s 0 2 in
  let sq2 = String.sub s 2 2 in
  if String.length s = 5 then
    let promote_id = piece_id_of_string (String.sub s 4 1) in
    ((sq1, sq2), Some promote_id)
  else ((sq1, sq2), None)

let best_move fen =
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
    Unix.open_process_full ("sh ./query_stockfish.sh " ^ bash_fen) [||]
  in
  close_out stdin;
  recover_output stdout |> parse_engine_response |> extract_engine_move
