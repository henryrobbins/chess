let parse_engine_response s =
  let strs = String.split_on_char ' ' s in
  match strs with
  | [ "bestmove"; move; "ponder"; next_move ] -> move
  | _ -> failwith "stockfish search failure"

let extract_engine_move s =
  let sq1 = String.sub s 0 2 in
  let sq2 = String.sub s 2 2 in
  (sq1, sq2)

let best_move fen =
  let recover_output channel =
    let rec loop h =
      try loop (input_line channel :: h) with End_of_file -> h
    in
    match loop [] with
    | [] -> failwith "stockfish init failure"
    | h :: t -> h :: t
  in
  let bash_fen = "'" ^ fen ^ "'" in
  let stdout, stdin, stderr =
    Unix.open_process_full ("sh ./test_uci_mac.sh " ^ bash_fen) [||]
  in
  close_out stdin;

  let rec print_list lst =
    match lst with
    | [] -> ()
    | h :: t -> print_endline h; print_list t; in

  print_list (recover_output stdout);
  print_endline (string_of_int (List.length (recover_output stdout)));

  (* recover_output stdout |> parse_engine_response |> extract_engine_move *)

(* bestmove e7d6 ponder c2c4 *)
