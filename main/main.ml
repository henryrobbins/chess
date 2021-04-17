open Board
open Command
open Validation

let string_of_string_tup_list tup_lst =
  let rev_lst = List.rev tup_lst in
  let rec build_str str lst' =
    match lst' with
    | [] -> str
    | (h, h') :: t ->
        build_str (str ^ "(" ^ h ^ ", " ^ h' ^ ")" ^ ", ") t
  in
  build_str "" rev_lst

let print_pins pin_lst =
  let rev_lst = List.rev pin_lst in
  let rec build_str str lst' =
    match lst' with
    | [] -> str
    | h :: t -> build_str (str ^ square_of_piece h ^ ", ") t
  in
  build_str "" rev_lst

let print_checkmate_stalemate board =
  if is_checkmate board then print_string "CHECKMATE \n"
  else if is_stalemate board then print_string "STALEMATE \n"
  else ();
  if is_checkmate board || is_stalemate board then exit 0 else ()

let prompt_for_move board =
  let color =
    match color_to_move board with White -> "White" | Black -> "Black"
  in
  print_string ("\n" ^ color ^ " to move  > ")

let print_invalid_move () =
  print_string "The move was invalid. Try again. \n"

let update_with_move_phrase board = function
  | [ id; sq; "to"; sq' ] ->
      let p =
        match piece_of_square board sq with
        | None -> failwith "never will happen"
        | Some p' -> p'
      in
      if is_valid_move (sq, sq') board then move_piece board p sq'
      else board
  | _ -> failwith "parse failed"

let rec interact board =
  print_game_state board;
  print_checkmate_stalemate board;
  prompt_for_move board;
  match read_line () with
  | exception End_of_file -> ()
  | text -> (
      try
        let command = parse text board in
        match command with
        | Quit -> exit 0
        | Move move_phrase ->
            let board' = update_with_move_phrase board move_phrase in
            if board = board' then print_invalid_move () else ();
            interact board'
      with
      | InconsistentPlacement ->
          print_string "placement \n";
          interact board
      | InvalidSquares ->
          print_string "invalid sq \n";
          interact board
      | Malformed ->
          print_string "malformed \n";
          interact board )

(** [main ()] prompts for the game to play, then starts it. *)
let main () = interact (init_game ())

(* Execute the game engine. *)
let () = main ()
