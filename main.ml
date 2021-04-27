open GMain
open GdkKeysyms
open Board
open Command
open Validation

(* Static parameters for the GUI *)
let width = 300

let height = 300

(** [string_of_string_list lst] TODO *)
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

(** [prompt_for_promotion] returns the piece type a user specifies for pawn
    promotion. *)
let prompt_for_promotion () =
  print_string ("\nEnter one of R, B, N, Q > ");
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
let update_with_move b m =
  match m with
  | sq, sq' ->
      let p =
        match piece_of_square b sq with
        | None -> failwith "impossible"
        | Some p' -> p'
      in
      if is_valid_move (sq, sq') b then
        let b' = (move_piece b p sq' true) in
        if is_pawn_promotion b p sq' then
          match piece_of_square b' sq' with
          | None -> failwith "impossible"
          | Some p' -> (promote_pawn b' p' (prompt_for_promotion ()))
        else b'
      else b

(** [command_line_turn] initiates a turn to be play chess via command
    line. *)
let rec command_line_turn board =
  print_game_state board;
  print_checkmate_stalemate board;
  prompt_for_move board;
  match read_line () with
  | exception End_of_file -> ()
  | text -> (
      try
        let move = move_from_phrase (parse text board) in
        let board' = update_with_move board move in
        if board = board' then print_invalid_move () else ();
        command_line_turn board'
      with
      | InconsistentPlacement ->
          print_string "placement \n";
          command_line_turn board
      | InvalidSquares ->
          print_string "invalid sq \n";
          command_line_turn board
      | Malformed ->
          print_string "malformed \n";
          command_line_turn board)

(** [command_line_main ()] initiates the game in command line mode. *)
let command_line_main () = command_line_turn (init_game ())

(* Connect a signal handler, ignoring the resulting signal ID. This
   avoids having to use [|> ignore] everywhere.
   https://stackoverflow.com/questions/63106011 *)
let ( ==> ) (signal : callback:_ -> GtkSignal.id) callback =
  ignore (signal ~callback)

(** [gui_main ()] initiates the game in gui mode. *)
let gui_main () =
  GtkMain.Main.init () |> ignore;
  let window =
    GWindow.window ~width ~height ~position:`CENTER ~resizable:true
      ~title:"OCaml Chess" ()
  in
  window#connect#destroy ==> Main.quit;
  let table = GPack.table ~packing:window#add () in
  let add i j x = table#attach i j x in
  let board_table =
    GPack.table ~packing:(add 0 0) () ~homogeneous:true
  in
  let vbox = GPack.vbox ~packing:(add 0 1) () in

  (* state variables *)
  let board = ref (init_game ()) in
  let choose_from = ref true in
  let from_sq = ref None in
  let to_sq = ref None in

  (* add file and rank labels *)
  let rec labels i =
    if i < 8 then (
      let add_file x = board_table#attach (i + 1) 8 x in
      let f_text = GMisc.label ~packing:add_file () in
      f_text#set_text (List.nth files i);
      f_text#set_justify `LEFT;
      let add_rank x = board_table#attach 0 (7 - i) x in
      let r_text = GMisc.label ~packing:add_rank () in
      r_text#set_text (List.nth ranks i);
      r_text#set_justify `LEFT;
      labels (i + 1))
  in
  labels 0;

  (* construct captured pieces labels *)
  let black_captured = GMisc.label ~packing:vbox#add () in
  black_captured#set_text "Black has Captured\n";
  let white_captured = GMisc.label ~packing:vbox#add () in
  white_captured#set_text "White has Captured\n";
  let turn = GMisc.label ~packing:vbox#add () in
  turn#set_text "White\n";

  (* construct button matrix *)
  let rec button_matrix rows cols i j btns =
    match rows with
    | [] -> btns
    | r :: rt -> (
        match cols with
        | [] ->
            if rt = [] then btns
            else button_matrix rt ranks (i + 1) 0 btns
        | c :: ct ->
            let id = string_of_piece (piece_of_square !board (r ^ c)) in
            let add x = board_table#attach i (7 - j) x in
            let button = GButton.button ~label:id ~packing:add () in
            let btns = (r ^ c, button) :: btns in
            button_matrix (r :: rt) ct i (j + 1) btns)
  in
  let buttons = button_matrix files ranks 1 0 [] in

  (* update labels *)
  let update_labels b =
    let print_lists = partition_pieces_by_color (captured_pieces b) in
    match print_lists with
    | lst, lst' ->
        let black_txt =
          "Black has Captured\n" ^ string_of_string_list lst
        in
        black_captured#set_text black_txt;
        let white_txt =
          "White has Captured\n" ^ string_of_string_list lst'
        in
        white_captured#set_text white_txt;
        let turn_txt =
          match color_to_move b with
          | White -> "White"
          | Black -> "Black"
        in
        turn#set_text turn_txt
  in

  (* update board *)
  let update_board b =
    let rec update_board_aux rows cols =
      match rows with
      | [] -> ()
      | r :: rt -> (
          match cols with
          | [] -> if rt = [] then () else update_board_aux rt ranks
          | c :: ct ->
              let sq = r ^ c in
              let button = List.assoc sq buttons in
              button#set_label (string_of_piece (piece_of_square b sq));
              update_board_aux (r :: rt) ct)
    in
    update_board_aux files ranks
  in

  (* go back and set all the button callbacks *)
  let rec set_callbacks rows cols =
    match rows with
    | [] -> ()
    | r :: rt -> (
        match cols with
        | [] -> if rt = [] then () else set_callbacks rt ranks
        | c :: ct ->
            let button = List.assoc (r ^ c) buttons in
            (* button#connect#enter ==> (fun () -> prerr_endline (r^c)); *)
            ( button#connect#pressed ==> fun () ->
              if !choose_from then (
                from_sq := Some (r ^ c);
                to_sq := None)
              else to_sq := Some (r ^ c);

              choose_from := not !choose_from;

              if !choose_from then
                (* get current from/to square and buttons *)
                let a =
                  match !from_sq with
                  | None -> failwith "no from sqaure selected"
                  | Some sq -> sq
                in
                let b =
                  match !to_sq with
                  | None -> failwith "no to square selected"
                  | Some sq -> sq
                in

                (* update *)
                let p = piece_of_square !board a in
                match p with
                | None -> print_endline "impossible"
                | Some p ->
                    let board' = update_with_move !board (a, b) in
                    if !board = board' then
                      print_endline "invalid move."
                    else (
                      board := board';
                      update_board board';
                      update_labels board');
                    print_endline
                      "==================TESTING==================";
                    print_game_state board';
                    print_checkmate_stalemate board';
                    print_endline
                      "===========================================" );
            set_callbacks (r :: rt) ct)
  in

  set_callbacks files ranks;

  window#show ();
  Main.main ()

(* Read argument to see which version of game to launch. *)
let () =
  match Sys.argv.(1) with
  | "command-line" -> command_line_main ()
  | "gui" -> gui_main ()
  | _ -> failwith "invalid game type"
