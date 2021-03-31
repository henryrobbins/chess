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

let rec interact board color =
  print_game_state board;
  print_string (string_of_string_tup_list (valid_moves color board));
  print_string "\n > ";
  let next_color = match color with Black -> White | White -> Black in
  match read_line () with
  | exception End_of_file -> ()
  | text -> (
      try
        let command = parse text board in
        match command with
        | Quit -> exit 0
        | Move move_phrase -> (
            match move_phrase with
            | [ id; sq; "to"; sq' ] ->
                let p =
                  match piece_of_square board sq with
                  | None -> failwith "never will happen"
                  | Some p' -> p'
                in
                if is_valid_move (sq, sq') board then
                  let board' = move_piece board p sq' in
                  interact board' next_color
                else print_string "The move was invalid. Try again. \n";
                interact board color
            | _ -> () )
      with
      | InconsistentPlacement ->
          print_string "placement";
          interact board color
      | InvalidSquares ->
          print_string "invalid sq";
          interact board color
      | Malformed ->
          print_string "malformed";
          interact board color )

(** [main ()] prompts for the game to play, then starts it. *)
let main () = interact (init_game ()) White

(* Execute the game engine. *)
let () = main ()
