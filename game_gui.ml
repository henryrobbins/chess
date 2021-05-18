open GMain
open Gdk
open GdkKeysyms
open Board
open Command
open Validation
open Unix
open Engine

(* Static parameters for the GUI *)
let width = 600
let height = 650

(* state variables *)
let board = ref (init_game ())
let choose_from = ref true
let from_sq = ref None
let to_sq = ref None

(* Connect a signal handler, ignoring the resulting signal ID. This
   avoids having to use [|> ignore] everywhere.
   https://stackoverflow.com/questions/63106011 *)
   let ( ==> ) (signal : callback:_ -> GtkSignal.id) callback =
   ignore (signal ~callback)

(** [sprite_pixbuf dim id] is the pixbuf containing the image of the
    sprite for the piece identifier [id] scaled to [dim]. *)
let sprite_pixbuf dim id =
  let pixbuf = GdkPixbuf.from_file ("assets/" ^ id ^ ".png") in
  let pixbuf' =
    GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true ()
  in
  GdkPixbuf.scale ~dest:pixbuf' ~width:dim ~height:dim pixbuf;
  pixbuf'

(** [update_button_image btn id] updates the button [btn] with the
    correct chess piece sprite for the given identifier [id]. *)
let update_button_image button id =
  if id = "  " then button#unset_image ()
  else
    GMisc.image ~pixbuf:(sprite_pixbuf 45 id) ~packing:button#set_image
      ()
    |> ignore

(** [pawn_promotion_window c] opens a window allowing player [c] to choose
    a piece to use in a pawn promotion. *)
let pawn_promotion_window color =
  GtkMain.Main.init () |> ignore;
  let d = 75 in
  let window =
    GWindow.window ~width:(d * 2) ~height:(d * 2) ~position:`CENTER
      ~resizable:true ~title:"Pawn Promotion" ()
  in
  window#connect#destroy ==> Main.quit;

  let c = String.uppercase_ascii (string_of_color color) in
  let table =
    GPack.table ~width:(d * 2) ~height:(d * 2) ~packing:window#add () in
  let add i j x = table#attach i j x in

  let btns = [("R", 0, 0); ("B", 1, 0); ("N", 0, 1); ("Q", 1, 1)] in
  let rec create_buttons btns =
    match btns with
    | [] -> ()
    | (pt, i, j) :: t ->
      let button = GButton.button ~packing:(add i j) () in
      GMisc.image ~pixbuf:(sprite_pixbuf (d - 10) (c ^ pt))
        ~packing:button#set_image () |> ignore;
      button#connect#pressed ==> fun () -> (
        print_endline pt;
        Main.quit ();
      );
      create_buttons t; in
  create_buttons btns;

  print_endline "done";

  window#show ();
  Main.main ()

(** [update_with_move b m] is the board [b] after executing move [m] if
    move [m] is valid. Otherwise, returns board [b] unaltered. *)
let update_with_move b m cmd =
  match m with
  | sq, sq' ->
      let p =
        match piece_of_square b sq with
        | None -> failwith "impossible"
        | Some p' -> p'
      in
      if is_valid_move (sq, sq') b then
        let b' = move_piece b p sq' true in
        if is_pawn_promotion b p sq' then
          match piece_of_square b' sq' with
          | None -> failwith "impossible"
          | Some p' ->
            pawn_promotion_window (color_to_move b);
            promote_pawn b' p' Queen
        else b'
      else b

(** [gui_main ()] initiates the game in gui mode. *)
let gui_main computer fen =
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
  let captured_table =
    GPack.table ~packing:(add 0 1) () ~homogeneous:true
  in
  let turn = GMisc.label ~packing:(add 0 2) () in
  let export_fen = GEdit.entry ~packing:(add 0 3) () in
  export_fen#set_editable false;

  (* state variables *)
  board := (try init_from_fen fen with Failure _ -> init_game ());

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
      labels (i + 1) )
  in
  labels 0;

  (* construct captured pieces labels *)
  let black_captured =
    GMisc.label ~packing:(fun x -> captured_table#attach 0 0 x) ()
  in
  black_captured#set_text "0";
  let white_captured =
    GMisc.label ~packing:(fun x -> captured_table#attach 0 1 x) ()
  in
  white_captured#set_text "0";
  (* TODO: Remove this eventually *)
  turn#set_text (string_of_color (color_to_move !board));
  export_fen#set_text (export_to_fen !board);

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
            let button = GButton.button ~packing:add () in

            let pixbuf =
              if (i + j) mod 2 = 1 then
                GdkPixbuf.from_file "assets/dark_sq.png"
              else GdkPixbuf.from_file "assets/light_sq.png"
            in
            let pixbuf' =
              GdkPixbuf.create ~width:60 ~height:60 ~has_alpha:true ()
            in
            GdkPixbuf.scale ~dest:pixbuf' ~width:60 ~height:60 pixbuf;
            GMisc.image ~pixbuf:pixbuf' ~packing:add () |> ignore;
            update_button_image button id;

            button#set_border_width 0;
            button#set_focus_on_click false;
            button#set_relief `NONE;

            let btns = (r ^ c, button) :: btns in
            button_matrix (r :: rt) ct i (j + 1) btns )
  in
  let buttons = button_matrix files ranks 1 0 [] in

  (* update labels *)
  let update_labels b =
    let print_lists = partition_pieces_by_color (captured_pieces b) in
    match print_lists with
    | lst, lst' ->
        black_captured#set_text
          (value_of_captured b White |> string_of_int);
        white_captured#set_text
          (value_of_captured b Black |> string_of_int);

        let rec add_captured_pieces i j pieces =
          match pieces with
          | [] -> ()
          | h :: t ->
              GMisc.image ~pixbuf:(sprite_pixbuf 30 h)
                ~packing:(fun x -> captured_table#attach j i x)
                ()
              |> ignore;
              add_captured_pieces i (j + 1) t
        in
        add_captured_pieces 0 1 (List.rev lst);
        add_captured_pieces 1 1 (List.rev lst');
        (* TODO: Remove this eventually *)
        turn#set_text (string_of_color (color_to_move b));
        export_fen#set_text (export_to_fen !board)
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
              let id = string_of_piece (piece_of_square b sq) in
              update_button_image button id;
              update_board_aux (r :: rt) ct )
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
            ( button#connect#enter ==> fun () ->
              if computer && color_to_move !board = Black then (
                let move = best_move (export_to_fen !board) in
                let board' = update_with_move !board (fst move) false in
                board := board';
                update_board board';
                update_labels board';
                print_endline
                  "==================TESTING==================";
                print_game_state board';
                print_endline
                  "===========================================" )
              else () );

            ( button#connect#pressed ==> fun () ->
              if !choose_from then (
                from_sq := Some (r ^ c);
                to_sq := None )
              else to_sq := Some (r ^ c);

              choose_from := not !choose_from;

              if !choose_from then
                (* get current from/to square and buttons *)
                let a =
                  match !from_sq with
                  | None -> failwith "no from square selected"
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
                    let move = ((a, b), None) in
                    let board' = update_with_move !board (fst move) false in
                    print_game_state board';
                    if !board = board' then
                      print_endline "invalid move."
                    else (
                      board := board';
                      update_board board';
                      update_labels board' );
                    print_endline
                      "==================TESTING==================";
                    print_game_state board';
                    print_endline
                      "===========================================" );

            set_callbacks (r :: rt) ct )
  in
  set_callbacks files ranks;

  window#show ();
  Main.main ()

(** [main ()] initiates the game in gui mode. *)
let main =
  GtkMain.Main.init () |> ignore;
  let window =
    GWindow.window ~width:250 ~height:100 ~position:`CENTER
      ~resizable:true ~title:"OCaml Chess" ()
  in
  window#connect#destroy ==> Main.quit;
  let table =
    GPack.table ~width:250 ~height:100 ~packing:window#add ()
  in
  let add i j x = table#attach i j x in
  let one_player_button = GButton.button ~packing:(add 0 0) () in
  one_player_button#set_label "One Player";
  let two_player_button = GButton.button ~packing:(add 0 1) () in
  two_player_button#set_label "Two Player";
  let fen = GEdit.entry ~packing:(add 0 2) () in
  fen#set_text "Paste an FEN here!";

  (* GEdit.combo_box ~packing:(add 0 3) () |> ignore; *)

  ( one_player_button#connect#pressed ==> fun () ->
    gui_main true fen#text );
  ( two_player_button#connect#pressed ==> fun () ->
    gui_main false fen#text );

  window#show ();
  Main.main ()

let () = main