open GMain
open Gdk
open GdkKeysyms
open Board
open Command
open Validation
open Unix
open Engine

(* Static parameters for the GUI *)
let width = 800
let height = 800

type game_window = {
  computer : bool; (* True if the user is playing aganist the computer. *)
  board : Board.t ref;  (* The current board state in this game window. *)
  drop : bool ref;  (* True if the user is selecting the to_square. *)
  promotion : (Board.p * square) option ref; (* Represents a promotion. *)
  to_square : square option ref;  (* Square piece should move to. *)
  from_square : square option ref;  (* Square piece is currently on. *)
  squares : ((int * int) * GButton.button) list; (* Widgets for squares. *)
  file_lbls : GMisc.label list; (* Widget for file labels. *)
  rank_lbls : GMisc.label list; (* Widget for rank labels. *)
  captured_table : GPack.table; (* Widget for captured pieces. *)
  black_captured : GMisc.label; (* Widget for black captured point value. *)
  white_captured : GMisc.label; (* Widget for white captured point value. *)
  (* piece_select : GPack.table; Widget for selecting pieces. *)
  export_fen : GEdit.entry; (* Widget for FEN for exporting. *)
  files : string list ref; (* Ordered list of files for board state. *)
  ranks : string list ref; (* Ordered list of ranks for board state. *)
}

let locale = GtkMain.Main.init ()

(** [extract option] extracts the value from option [option] if it exists.
    Otherwise, fails with "impossible". *)
let extract = function
  | None -> failwith "impossible"
  | Some option -> option

(* Connect a signal handler, ignoring the resulting signal ID. This
   avoids having to use [|> ignore] everywhere.
   https://stackoverflow.com/questions/63106011 *)
let ( ==> ) (signal : callback:_ -> GtkSignal.id) callback =
  ignore (signal ~callback)

(** [sprite d name] is the sprite called [name] scaled to dimension [d]. *)
let sprite d name =
  let pixbuf = GdkPixbuf.from_file ("assets/" ^ name ^ ".png") in
  let pixbuf' =
    GdkPixbuf.create ~width:d ~height:d ~has_alpha:true ()
  in
  GdkPixbuf.scale ~dest:pixbuf' ~width:d ~height:d pixbuf;
  pixbuf'

(** [update_button_image btn id] updates the button [btn] with the
    correct chess piece sprite for the given identifier [id]. *)
let update_button_image button id =
  if id = "  " then button#unset_image ()
  else
    GMisc.image ~pixbuf:(sprite 45 id) ~packing:button#set_image () |> ignore;
    (* GMisc.image ~pixbuf:(sprite 45 "dot") ~packing:button#add () |> ignore; *)
  ()

(** [add_file_rank_labels t] adds file and rank labels to a game board [t]. *)
let add_file_rank_labels (table : GPack.table) =
  let add i j x = table#attach i j x in
  let rec labels_aux i f r =
    if i < 8 then (
      let f_text = GMisc.label ~packing:(add (i + 1) 8) () in
      f_text#set_text (List.nth files i);
      f_text#set_justify `CENTER;
      let r_text = GMisc.label ~packing:(add 0 (7 - i)) () in
      r_text#set_text (List.nth ranks i);
      r_text#set_justify `CENTER;
      labels_aux (i + 1) (f_text :: f) (r_text :: r))
    else (List.rev f, List.rev r)
  in
  labels_aux 0 [] []

(** [add_board_squares b t] adds buttons for board squares to game board [t]
    representing the board state [b]. *)
let add_board_squares board (table : GPack.table) =
  let rec squares_aux i j btns =
    if i < 8 then
      if j = 8 then (if i = 7 then btns else squares_aux (i + 1) 0 btns)
      else
        let r = List.nth files i in
        let c = List.nth ranks j in
        let id = string_of_piece (piece_of_square !board (r ^ c)) in
        let add x = table#attach (i + 1) (7 - j) x in
        let button = GButton.button ~packing:add () in
        let name = if (i + j) mod 2 = 1 then "dark_sq" else "light_sq" in
        GMisc.image ~pixbuf:(sprite 60 name) ~packing:add () |> ignore;
        update_button_image button id;
        button#set_border_width 0;
        button#set_focus_on_click false;
        button#set_relief `NONE;
        let btns = ((i,j), button) :: btns in
        squares_aux i (j + 1) btns
    else btns
  in
  squares_aux 0 0 []

(** [init_captured_table] returns a table widget for captured pieces and the
    labels for updating point values. *)
let init_captured_table packing =
  let table = GPack.table ~packing:packing ~homogeneous:true () in
  let add i j x = table#attach i j x in
  let black_captured = GMisc.label ~packing:(add 0 0) () in
  black_captured#set_text "0";
  let white_captured = GMisc.label ~packing:(add 0 1) () in
  white_captured#set_text "0";
  (table, black_captured, white_captured)

(** [update_board b buttons] updates the playing board [buttons] with the
    current board state [b]. *)
let update_board w =
  let rec update_board_aux i j =
    if i < 8 then
      if j = 8 then (if i = 7 then () else update_board_aux (i + 1) 0)
      else
        let sq = (List.nth !(w.files) i) ^ (List.nth !(w.ranks) j) in
        let button = List.assoc (i,j) w.squares in
        let id = string_of_piece (piece_of_square !(w.board) sq) in
        update_button_image button id;
        update_board_aux i (j + 1)
  in
  update_board_aux 0 0

(** [update_file_rank_labels w] updates file and rank labels for window [w]. *)
let update_file_rank_labels w =
  let rec aux i =
    if i < 8 then (
      (List.nth w.file_lbls i)#set_text (List.nth !(w.files) i);
      (List.nth w.rank_lbls i)#set_text (List.nth !(w.ranks) i);
      aux (i + 1);) in
  aux 0; ()

(** [update_captured b t black white] updates the table of captured pieces [t],
    black captured point value [black], and white captured point value [white]
    for the given board state [b]. *)
let update_captured w =
  let b = !(w.board) in
  let print_lists = partition_pieces_by_color (captured_pieces b) in
  match print_lists with
  | lst, lst' -> (
    w.black_captured#set_text (value_of_captured b White |> string_of_int);
    w.white_captured#set_text (value_of_captured b Black |> string_of_int);
    let rec add_captured_pieces i j pieces =
      match pieces with
      | [] -> ()
      | h :: t ->
        GMisc.image ~pixbuf:(sprite 30 h)
          ~packing:(fun x -> w.captured_table#attach j i x)
          ()
        |> ignore;
        add_captured_pieces i (j + 1) t
    in
    add_captured_pieces 0 1 (List.rev lst);
    add_captured_pieces 1 1 (List.rev lst');)

(** [update_ranks_and_files w] updates the ranks and files. *)
let update_ranks_and_files w =
  if w.computer || color_to_move !(w.board) = White then (
    w.files := files;
    w.ranks := ranks;)
  else (
    w.files := List.rev files;
    w.ranks := List.rev ranks;)

(** [update_window b ...] updates all the widgets on the window for the given
    board state [b]. *)
let update_window w =
  update_ranks_and_files w;
  update_file_rank_labels w;
  update_captured w;
  update_board w;
  let board = !(w.board) in
  w.export_fen#set_text (export_to_fen board); ()

(** [terminal_output b] sends output to teminal representing the state [b]. *)
let terminal_output b =
    print_endline
    "===========================================";
    print_game_state b;
    print_endline
    "===========================================";
    if is_checkmate b then print_string "CHECKMATE \n"
    else if is_stalemate b then print_string "STALEMATE \n"
    else (); ()

(** [piece_select_callback pt] is the callback function when pressing the
    button corresponding to piece type [pt] in the piece select window. *)
let piece_select_callback w pt = fun () -> (
  match !(w.promotion) with
  | None -> ()
  | Some (p, sq') ->
    let b' = move_piece !(w.board) p sq' true in
    let p' = extract (piece_of_square b' sq') in
    w.board := promote_pawn b' p' pt;
    w.promotion := None;
    update_window w;
)

(** [init_piece_selection packing] adds a widget for piece selection using
    the given packing function [packing]. *)
let init_piece_selection w packing =
  let d = 60 in
  let c = String.uppercase_ascii (string_of_color White) in
  let table = GPack.table ~width:(d*2) ~height:(d*2) ~packing:packing () in
  let add i j x = table#attach i j x in
  let attr = [(Rook, 0, 0); (Bishop, 0, 1); (Knight, 0, 2); (Queen, 0, 3)] in
  let rec create_buttons attr =
    match attr with
    | [] -> ()
    | (pt, i, j) :: t ->
      GMisc.image ~pixbuf:(sprite 60 "gray_sq") ~packing:(add i j) |> ignore;
      let button = GButton.button ~packing:(add i j) () in
      button#set_border_width 0;
      button#set_focus_on_click false;
      button#set_relief `NONE;
      GMisc.image ~pixbuf:(sprite (d - 15) (c ^ (string_of_piece_id pt)))
        ~packing:button#set_image () |> ignore;
      button#connect#pressed ==> piece_select_callback w pt;
      create_buttons t; in
  create_buttons attr;
  table

(** [enter_square_callback w] is the callback function for window [w] called
    when the mouse enters a square. *)
let enter_square_callback w = fun () -> (
  let b = !(w.board) in
  let in_promotion =
    match !(w.promotion) with
    | None -> false
    | Some _ -> true in
  if not in_promotion && w.computer && color_to_move b = Black then (
    let (sq, sq'), promote = best_move (export_to_fen b) in
    let p = extract (piece_of_square b sq) in
    let board' = move_piece b p sq' true in
    let board' =
    match promote with
    | None -> board'
    | Some pt -> (
      let p' = extract (piece_of_square board' sq') in
      promote_pawn board' p' pt) in
    w.board := board';
    update_window w;
    terminal_output board' )
  else ()
)

(** [index_of list a] returns the index of the element [a] in list [list] if it
    exists. Otherwise, returns -1. *)
let index_of list a =
  let rec index_of_aux lst i =
    match lst with
    | [] -> -1
    | h :: t -> if a = h then i else index_of_aux t (i + 1) in
  index_of_aux list 0

(** [show_valid_moves w p] updates window [w] to show all of the valid moves
    for the current selected piece [p]. *)
let show_valid_moves w p =
  update_window w;
  let rec show_valid_moves_aux lst =
    match lst with
    | [] -> ()
    | (_,sq) :: t -> (
      let i = index_of !(w.files) (Char.escaped sq.[0]) in
      let j = index_of !(w.ranks) (Char.escaped sq.[1]) in
      let button = List.assoc (i,j) w.squares in
      GMisc.image ~pixbuf:(sprite 45 "dot") ~packing:button#set_image () |> ignore;
      show_valid_moves_aux t;);
  in
  show_valid_moves_aux (valid_piece_moves !(w.board) p); ()

(** [from_square_callback w sq p] is the callback function for window [w]
    called when the mouse selects the from_square [sq] with the piece [p] on
    it for a move. *)
let from_square_callback w sq p =
  w.drop := true;
  w.from_square := Some sq;
  w.to_square := None;
  show_valid_moves w (extract p); ()

(** [to_square_callback w sq] is the callback function for window [w] called
    when the mouse selects the from_square [sq] for a move. *)
let to_square_callback w sq =
  let b = !(w.board) in
  let sq' = sq in
  let sq = extract (!(w.from_square)) in
  let p = extract (piece_of_square b sq) in
  let board' =
    if is_valid_move (sq, sq') b then (
      if is_pawn_promotion b p sq' then
       ( w.promotion := Some (p, sq'); b)
      else move_piece b p sq' true)
    else b in
  w.drop := false;
  w.from_square := None;
  w.to_square := None;
  w.board := board';
  update_window w;
  terminal_output board'; ()

(** [pressed_square_callback w btn] is the callback function for window [w]
    called when the mouse presses on a the button [btn]. *)
let pressed_square_callback w btn = fun () -> (
  match !(w.promotion) with
  | Some _ -> ()
  | None ->
    let (i,j) = btn in
    let r = List.nth !(w.files) i in
    let c = List.nth !(w.ranks) j in
    let sq = r ^ c in
    let p = piece_of_square !(w.board) sq in
    let valid_start =
      match p with
      | None -> false
      | Some p -> (color_of_piece p) = color_to_move !(w.board) in
    if not !(w.drop) || valid_start then (
      if valid_start then from_square_callback w sq p else ()
    )
    else to_square_callback w sq;)

(** [gui_main ()] initiates the game in gui mode. *)
let gui_main computer fen =
  (* main game window *)
  let window =
    GWindow.window ~width ~height ~position:`CENTER ~resizable:true
      ~title:"OCaml Chess" ()
  in
  window#connect#destroy ==> Main.quit;

  (* state variables *)
  let board = ref (try init_from_fen fen with Failure _ -> init_game ()) in
  let drop = ref false in
  let promotion = ref None in
  let from_square = ref None in
  let to_square = ref None in
  let files = ref files in
  let ranks = ref ranks in

  (* container for all widgets *)
  let table = GPack.table ~packing:window#add () in
  table#set_col_spacings 20;
  table#set_row_spacings 20;

  let add i j x = table#attach i j x in

  (* widgets to add to main container *)

  let board_table = GPack.table ~packing:(add 0 0) ~homogeneous:true () in
  let file_lbls, rank_lbls = add_file_rank_labels board_table in
  let squares = add_board_squares board board_table in

  let captured_table, black_captured, white_captured =
    init_captured_table (add 0 1) in

  let export_fen = GEdit.entry ~packing:(add 0 2) () in
  export_fen#set_text (export_to_fen !board);
  export_fen#set_editable false;

  let game_window = {
    computer; board; drop; promotion; from_square; to_square; squares;
    captured_table; black_captured; white_captured; export_fen; files; ranks;
    file_lbls; rank_lbls
  } in

  init_piece_selection game_window (add 1 0) |> ignore;

  (* go back and set all the button callbacks *)
  let rec set_callbacks i j =
    if i < 8 then
      if j = 8 then (if i = 7 then () else set_callbacks (i + 1) 0)
      else
        let button = List.assoc (i,j) squares in
        button#connect#enter ==> enter_square_callback game_window;
        button#connect#pressed ==> pressed_square_callback game_window (i,j);
        set_callbacks i (j + 1);
  in
  set_callbacks 0 0;

  window#show ();
  Main.main ()

(** [main ()] initiates the game in gui mode. *)
let main =
  (* GtkMain.Main.init () |> ignore; *)
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

  ( one_player_button#connect#pressed ==> fun () ->
    gui_main true fen#text );
  ( two_player_button#connect#pressed ==> fun () ->
    gui_main false fen#text );

  window#show ();
  Main.main ()

let () = main
