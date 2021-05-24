open Board
open Command
open Validation
open Endgame

(** [string_of_string_list lst] is a string representing a list of strings. *)
let string_of_string_list lst =
  let rev_lst = List.rev lst in
  let rec build_str str lst' =
    match lst' with [] -> str | h :: t -> build_str (str ^ h ^ ", ") t
  in
  build_str "" rev_lst

(** [print_checkmate_stalemate b] prints "CHECKMATE" or "STALEMATE" and
    exits the game iff the game state [b] is in the respective state. *)
let print_checkmate_stalemate b =
  if is_checkmate b then print_string "CHECKMATE \n"
  else if is_stalemate b then print_string "STALEMATE \n"
  else ();
  if is_checkmate b || is_stalemate b then exit 0 else ()

(** [prompt_for_move b] prints a prompt to the player whose turn it is
    in the game state [b]. *)
let prompt_for_move b =
  let color =
    match color_to_move b with White -> "White" | Black -> "Black"
  in
  print_string ("\n" ^ color ^ " to move  > ")

(** [prompt_for_promotion] returns the piece type a user specifies for
    pawn promotion. *)
let prompt_for_promotion () =
  print_string "\nEnter one of R, B, N, Q > ";
  match read_line () with
  | exception End_of_file -> failwith "no input"
  | text -> piece_id_of_string text

(** [print_invalid_move] prints an indication that a move was invalid. *)
let print_invalid_move () =
  print_string "The move was invalid. Try again. \n"

(** [move_from_phrase phrase] is the move from the move phrase [phrase]. *)
let move_from_phrase = function
  | Quit -> exit 0
  | Move [ id; sq; _; sq' ] -> (sq, sq')
  | _ -> failwith "parse failed"

(** [update_with_move b m] is the board [b] after executing move [m] if
    move [m] is valid. Otherwise, returns board [b] unaltered. *)
let update_with_move b m cmd =
  match m with
  | sq, sq' ->
    let p = Option.get ( piece_of_square b sq) in
    if not (is_valid_move (sq, sq') b) then b else
    let b' = move_piece b p sq' true in
    if not (is_pawn_promotion b p sq') then b' else
    let p' = Option.get (piece_of_square b' sq') in
    promote_pawn b' p' (prompt_for_promotion ())

(** [turn] initiates a turn to be play chess via command line. *)
let rec turn board =
  print_game_state board;
  print_checkmate_stalemate board;
  prompt_for_move board;
  match read_line () with
  | exception End_of_file -> ()
  | text -> parse_user_input text board

(** [parse_user_input text board] parses the user input. *)
and parse_user_input text board =
  try
    let move = move_from_phrase (parse text board) in
    let board' = update_with_move board move true in
    if board = board' then print_invalid_move () else ();
    turn board'
  with
  | InconsistentPlacement -> print_string "placement \n"; turn board
  | InvalidSquares -> print_string "invalid sq \n"; turn board
  | Malformed -> print_string "malformed \n"; turn board

let () = turn (init_game ())
