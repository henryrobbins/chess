open GMain
open GdkKeysyms
open Board
open Command
open Validation

(* Static parameters for the GUI *)
let width = 300
let height = 00

(* TODO: Decide how to remove duplication of this code from main.ml *)

let string_of_string_list lst =
  let rev_lst = List.rev lst in
  let rec build_str str lst' =
    match lst' with [] -> str | h :: t -> build_str (str ^ h ^ ", ") t
  in
  build_str "" rev_lst

let print_checkmate_stalemate board =
  if is_checkmate board then print_string "CHECKMATE \n"
  else if is_stalemate board then print_string "STALEMATE \n"
  else ();
  if is_checkmate board || is_stalemate board then exit 0 else ()

let update_with_move_phrase board sq sq' =
    let p =
      match piece_of_square board sq with
      | None -> failwith "never will happen"
      | Some p' -> p'
    in
    if is_valid_move (sq, sq') board then move_piece board p sq'
    else board

(* Connect a signal handler, ignoring the resulting signal ID.
   This avoids having to use [|> ignore] everywhere.
   https://stackoverflow.com/questions/63106011 *)
let (==>) (signal:(callback:_ -> GtkSignal.id)) callback =
   ignore (signal ~callback)

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:width ~height:height
                              ~position: `CENTER
                              ~resizable: true
                              ~title:"OCaml Chess" () in
  let table = GPack.table ~packing:window#add () in
  let add i j x = table#attach i j x in
  let board_table = GPack.table ~packing:(add 0 0) ()
                                ~homogeneous: true
                                ~width: width ~height: height in
  let vbox = GPack.vbox ~packing:(add 0 1) () ~width: width in
  window#connect#destroy  ==> Main.quit;

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
      labels (i + 1)) in
  labels 0;

  (* captured pieces *)
  let black_captured = GMisc.label ~packing:vbox#add () ~width: width in
  black_captured#set_text "Black has Captured\n";
  let white_captured = GMisc.label ~packing:vbox#add () ~width: width in
  white_captured#set_text "White has Captured\n";

  let update_captured b =
    let print_lists = partition_pieces_by_color (captured_pieces b) in
    match print_lists with
    | lst, lst' ->
        let black_txt = "Black has Captured\n"^(string_of_string_list lst) in
        black_captured#set_text black_txt;
        let white_txt = "White has Captured\n"^(string_of_string_list lst') in
        white_captured#set_text white_txt; in

  (* construct button matrix *)
  let rec button_matrix rows cols i j btns =
    match rows with
    | [] -> btns;
    | r :: rt ->
      match cols with
      | [] ->
        if rt = [] then btns else button_matrix rt ranks (i + 1) 0 btns
      | c :: ct ->
        let id = print_piece (piece_of_square !board (r^c)) in
        let add x = board_table#attach i (7 - j) x in
        let button = GButton.button ~label:id ~packing:add () in
        let btns = (r^c, button) :: btns in
        button_matrix (r :: rt) ct i (j + 1) btns in

  let buttons = button_matrix files ranks 1 0 [] in

  (* go back and set all the button callbacks *)
  let rec set_callbacks rows cols =
    match rows with
    | [] -> ();
    | r :: rt ->
      match cols with
      | [] -> if rt = [] then () else set_callbacks rt ranks
      | c :: ct ->
        let button = List.assoc (r^c) buttons in
        (* button#connect#enter  ==> (fun () -> prerr_endline (r^c)); *)
        button#connect#pressed  ==> (fun () ->
          if !choose_from then
            (from_sq := Some (r^c); to_sq := None)
          else
            to_sq := Some (r^c);

          choose_from := not !choose_from;

          if !choose_from then
            (* get current from/to square and buttons *)
            let a =
              match !from_sq with
              | None -> failwith "no from sqaure selected"
              | Some sq -> sq in
            let b =
              match !to_sq with
              | None -> failwith "no to square selected"
              | Some sq -> sq in
            let from_btn = List.assoc a buttons in
            let to_btn = List.assoc b buttons in

            (* update *)
            let p = piece_of_square !board a in
            match p with
            | None -> print_endline "impossible"
            | Some p ->
              let board' = update_with_move_phrase !board a b in
              if !board = board' then
                print_endline "invalid move."
              else (
                board := board';
                from_btn#set_label (print_piece (piece_of_square board' a));
                to_btn#set_label (print_piece (piece_of_square board' b));
                update_captured board');
          print_endline "==================TESTING==================";
          print_game_state board';
          print_checkmate_stalemate board';
          print_endline "===========================================";
        );
        set_callbacks (r :: rt) ct in

  set_callbacks files ranks;

  window#show ();
  Main.main ()

let () = main ()
