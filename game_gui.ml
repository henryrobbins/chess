open GMain
open Gdk
open GdkKeysyms
open Board
open Command
open Validation
open Unix
open Engine
open Endgame
open Puzzle

(* Static parameters for the GUI *)
let width = 640

let height = 700

let rgb_color r g b = [ (`NORMAL, `RGB (r * 255, g * 255, b * 255)) ]

let bg_color = rgb_color 37 35 32

let text_color = rgb_color 200 200 200

let dark_color = rgb_color 63 31 31

let light_color = rgb_color 104 59 47

type mode =
  | SinglePlayer
  | TwoPlayer
  | Rush

type game_window = {
  mode : mode;
  (* The game mode of this window. *)
  rush : rush option;
  (* The elo of the opposing player if single player is chosen *)
  elo : string;
  (* An instance of rush if the mode is rush. *)
  board : Board.t ref;
  (* The current board state in this game window. *)
  locked : bool ref;
  (* True if the window is now locked. *)
  computer : color option ref;
  (* Color of the computer player (if not two player) *)
  drop : bool ref;
  (* True if the user is selecting the to_square. *)
  promotion : (Board.p * square) option ref;
  (* Represents a promotion. *)
  to_square : square option ref;
  (* Square piece should move to. *)
  from_square : square option ref;
  (* Square piece is currently on. *)
  squares : ((int * int) * GButton.button) list;
  (* Widgets for squares. *)
  file_lbls : GMisc.label list;
  (* Widget for file labels. *)
  rank_lbls : GMisc.label list;
  (* Widget for rank labels. *)
  captured_table : GPack.table;
  (* Widget for captured pieces. *)
  black_captured : GMisc.label;
  (* Widget for black captured point value. *)
  white_captured : GMisc.label;
  (* Widget for white captured point value. *)
  piece_select : (Board.piece_type * GButton.button) list;
  (* Select type. *)
  export_fen : GEdit.entry;
  (* Widget for FEN for exporting. *)
  files : string list ref;
  (* Ordered list of files for board state. *)
  ranks : string list ref;
  (* Ordered list of ranks for board state. *)
  total_solved : GMisc.label;
  (* Widget for the number of puzzles solved. *)
  total_wrong : GMisc.label;
      (* Widget for the number of puzzles wrong. *)
}

let locale = GtkMain.Main.init ()

(** [extract option] extracts the value from option [option] if it
    exists. Otherwise, fails with "impossible". *)
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
    GMisc.image ~pixbuf:(sprite 45 id) ~packing:button#set_image ()
    |> ignore;
  ()

(** [text_label text packing] is a text label with initial text [text]
    and size [size] using the given packing function [packing]. *)
let text_label text size packing =
  let lbl = GMisc.label ~packing () in
  lbl#set_text ("<b>" ^ text ^ "</b>");
  lbl#set_use_markup true;
  lbl#misc#modify_font_by_name ("Sans " ^ string_of_int size);
  lbl#misc#modify_fg text_color;
  lbl#set_justify `CENTER;
  lbl

(** [update_text_label lbl t] updates the text [text] of the label
    [label] *)
let update_text_label label text =
  label#set_text ("<b>" ^ text ^ "</b>");
  label#set_use_markup true;
  ()

(** [add_file_rank_labels t] adds file and rank labels to a game board
    [t]. *)
let add_file_rank_labels (table : GPack.table) =
  let add i j x = table#attach i j x in
  let rec labels_aux i f r =
    if i < 8 then
      let f_text = text_label (List.nth files i) 16 (add (i + 1) 8) in
      let r_text = text_label (List.nth ranks i) 16 (add 0 (7 - i)) in
      labels_aux (i + 1) (f_text :: f) (r_text :: r)
    else (List.rev f, List.rev r)
  in
  labels_aux 0 [] []

(** [add_board_squares b t] adds buttons for board squares to game board
    [t] representing the board state [b]. *)
let add_board_squares board (table : GPack.table) =
  let rec squares_aux i j btns =
    if i < 8 then (
      if j = 8 then if i = 7 then btns else squares_aux (i + 1) 0 btns
      else
        let r = List.nth files i in
        let c = List.nth ranks j in
        let id = string_of_piece (piece_of_square !board (r ^ c)) in
        let add x = table#attach (i + 1) (7 - j) x in
        let button = GButton.button ~packing:add () in
        let name =
          if (i + j) mod 2 = 1 then "light_sq" else "dark_sq"
        in
        GMisc.image ~pixbuf:(sprite 60 name) ~packing:add () |> ignore;
        update_button_image button id;
        button#set_border_width 0;
        button#set_focus_on_click false;
        button#set_relief `NONE;
        let btns = ((i, j), button) :: btns in
        squares_aux i (j + 1) btns )
    else btns
  in
  squares_aux 0 0 []

(** [init_captured_table] returns a table widget for captured pieces and
    the labels for updating point values. *)
let init_captured_table packing =
  let table = GPack.table ~packing ~homogeneous:true () in
  let add i j x = table#attach i j x in
  let black_captured = text_label "0" 14 (add 0 0) in
  let white_captured = text_label "0" 14 (add 0 1) in
  (table, black_captured, white_captured)

(** [init_piece_selection packing] adds a widget for piece selection
    using the given packing function [packing]. *)
let init_piece_selection packing =
  let d = 60 in
  let c = String.uppercase_ascii (string_of_color White) in
  let table = GPack.table ~width:(d * 2) ~height:(d * 2) ~packing () in
  let add i j x = table#attach i j x in
  let attr =
    [ (Rook, 0, 0); (Bishop, 0, 1); (Knight, 0, 2); (Queen, 0, 3) ]
  in
  let rec create_buttons attr btns =
    match attr with
    | [] -> btns
    | (pt, i, j) :: t ->
        let button = GButton.button ~packing:(add i j) () in
        button#set_border_width 0;
        button#set_focus_on_click false;
        button#set_relief `NONE;
        button#misc#hide ();
        GMisc.image
          ~pixbuf:(sprite (d - 15) (c ^ string_of_piece_id pt))
          ~packing:button#set_image ()
        |> ignore;
        create_buttons t ((pt, button) :: btns)
  in
  create_buttons attr []

(** [init_puzzle_labels packing] adds a widget for puzzle labels using
    the given packing function [packing]. *)
let init_puzzle_labels packing =
  let d = 60 in
  let table =
    GPack.table ~width:(d * 2) ~height:(d * 2) ~packing ()
      ~homogeneous:true
  in
  table#set_col_spacings 40;
  table#set_row_spacings 20;
  let add i j x = table#attach i j x in
  text_label "   <u>Total Solved</u>    " 16 (add 1 0) |> ignore;
  text_label "   <u>Total Wrong</u>     " 16 (add 2 0) |> ignore;
  let solved = text_label "0" 16 (add 1 1) in
  let total_wrong = text_label "0" 16 (add 2 1) in
  (table, solved, total_wrong)

(** [update_board b buttons] updates the playing board [buttons] with
    the current board state [b]. *)
let update_board w =
  let rec update_board_aux i j =
    if i < 8 then (
      if j = 8 then if i = 7 then () else update_board_aux (i + 1) 0
      else
        let sq = List.nth !(w.files) i ^ List.nth !(w.ranks) j in
        let button = List.assoc (i, j) w.squares in
        let id = string_of_piece (piece_of_square !(w.board) sq) in
        update_button_image button id;
        update_board_aux i (j + 1) )
  in
  update_board_aux 0 0

(** [update_file_rank_labels w] updates file and rank labels for window
    [w]. *)
let update_file_rank_labels w =
  let rec aux i =
    if i < 8 then (
      update_text_label (List.nth w.file_lbls i) (List.nth !(w.files) i);
      update_text_label (List.nth w.rank_lbls i) (List.nth !(w.ranks) i);
      aux (i + 1) )
  in
  aux 0;
  ()

(** [update_captured b t black white] updates the table of captured
    pieces [t], black captured point value [black], and white captured
    point value [white] for the given board state [b]. *)
let update_captured w =
  let b = !(w.board) in
  let print_lists = partition_pieces_by_color (captured_pieces b) in
  match print_lists with
  | lst, lst' ->
      let black_cap_text = value_of_captured b White |> string_of_int in
      update_text_label w.black_captured black_cap_text;
      let white_cap_text = value_of_captured b Black |> string_of_int in
      update_text_label w.white_captured white_cap_text;
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
      add_captured_pieces 1 1 (List.rev lst')

(** [update_ranks_and_files w] updates the ranks and files. *)
let update_ranks_and_files w =
  if (not (w.mode = TwoPlayer)) || color_to_move !(w.board) = White then (
    w.files := files;
    w.ranks := ranks )
  else (
    w.files := List.rev files;
    w.ranks := List.rev ranks )

(** [update_piece_select w] updates the piece select menu. *)
let update_piece_select w =
  let rec aux pts =
    match pts with
    | [] -> ()
    | h :: t ->
        let button = List.assoc h w.piece_select in
        let c = string_of_color (color_to_move !(w.board)) in
        let id = String.uppercase_ascii c ^ string_of_piece_id h in
        GMisc.image ~pixbuf:(sprite 45 id) ~packing:button#set_image ()
        |> ignore;
        ( match !(w.promotion) with
        | None -> button#misc#hide ()
        | _ -> button#misc#show () );
        aux t
  in
  aux [ Rook; Bishop; Knight; Queen ];
  ()

(** [update_rush_labels w] updates the total solved and wrong labels. *)
let update_rush_labels w =
  let rush = extract w.rush in
  update_text_label w.total_solved
    (rush |> total_solved |> string_of_int);
  update_text_label w.total_wrong (rush |> total_wrong |> string_of_int);
  ()

(** [text_popup text] is a popup window with the text [text] on it. *)
let text_popup text =
  let window =
    GWindow.window ~width:250 ~height:100 ~position:`CENTER
      ~resizable:true ~title:"OCaml Chess" ()
  in
  window#connect#destroy ==> Main.quit;
  window#misc#modify_bg bg_color;
  text_label text 16 window#add |> ignore;
  window#show ();
  Main.main ()

(** [update_window b ...] updates all the widgets on the window for the
    given board state [b]. *)
let update_window w =
  update_ranks_and_files w;
  update_file_rank_labels w;
  update_piece_select w;
  update_captured w;
  update_board w;
  let b = !(w.board) in
  w.export_fen#set_text (export_to_fen b);
  match w.mode with
  | SinglePlayer | TwoPlayer ->
      if is_checkmate b then (
        text_popup "CHECKMATE";
        w.locked := true );
      if is_stalemate b then (
        text_popup "STALEMATE";
        w.locked := true );
      if is_draw b then (
        text_popup "DRAW";
        w.locked := true )
  | Rush -> update_rush_labels w

(** [terminal_output b] sends output to teminal representing the state
    [b]. *)
let terminal_output b = ()

(* print_endline "===========================================";
   print_game_state b; print_endline
   "==========================================="; if is_checkmate b then
   print_string "CHECKMATE \n" else if is_stalemate b then print_string
   "STALEMATE \n" else () *)

(** [piece_select_callback pt] is the callback function when pressing
    the button corresponding to piece type [pt] in the piece select
    window. *)
let piece_select_callback w pt () =
  match !(w.promotion) with
  | None -> ()
  | Some (p, sq') ->
      let b' = move_piece !(w.board) p sq' true in
      let p' = extract (piece_of_square b' sq') in
      w.board := promote_pawn b' p' pt;
      w.promotion := None;
      update_window w

(** [rush_callback w] is the callback function for a rush game. *)
let rush_pressed_callback w =
  let current_fen = export_to_fen !(w.board) in
  let rush = extract w.rush in
  let progress = update_rush_with_move rush current_fen in
  update_rush_labels w;
  w.locked := true;
  ( match progress with
  | Complete -> text_popup "You Win!"
  | GameOver -> text_popup "Game Over!"
  | Correct ->
      text_popup "Correct! Next Puzzle.";
      w.computer := Some (computer_color rush);
      w.locked := false
  | Wrong ->
      text_popup "Incorrect. Next Puzzle.";
      w.computer := Some (computer_color rush);
      w.locked := false
  | InProgress -> w.locked := false );
  w.board := current_board rush

(** [single_player_callback w] is the callback function for a single
    player game. *)
let single_player_callback w =
  let b = !(w.board) in
  let in_promotion =
    match !(w.promotion) with None -> false | Some _ -> true
  in
  if
    (not in_promotion) && w.mode = SinglePlayer
    && color_to_move b = Black
  then
    let (sq, sq'), promote = best_move (export_to_fen b) w.elo in
    let p = extract (piece_of_square b sq) in
    let board' = move_piece b p sq' true in
    let board' =
      match promote with
      | None -> board'
      | Some pt ->
          let p' = extract (piece_of_square board' sq') in
          promote_pawn board' p' pt
    in
    w.board := board'

(** [enter_square_callback w] is the callback function for window [w]
    called when the mouse enters a square. *)
let enter_square_callback w () =
  match w.mode with
  | TwoPlayer -> ()
  | _ ->
      let cc = extract !(w.computer) in
      if (not !(w.locked)) && color_to_move !(w.board) = cc then (
        ( match w.mode with
        | SinglePlayer -> single_player_callback w
        | TwoPlayer -> ()
        | Rush -> rush_pressed_callback w );
        update_window w;
        terminal_output !(w.board) )
      else ()

(** [index_of list a] returns the index of the element [a] in list
    [list] if it exists. Otherwise, returns -1. *)
let index_of list a =
  let rec index_of_aux lst i =
    match lst with
    | [] -> -1
    | h :: t -> if a = h then i else index_of_aux t (i + 1)
  in
  index_of_aux list 0

(** [show_valid_moves w p] updates window [w] to show all of the valid
    moves for the current selected piece [p]. *)
let show_valid_moves w p =
  update_window w;
  let rec show_valid_moves_aux lst =
    match lst with
    | [] -> ()
    | (_, sq) :: t ->
        let i = index_of !(w.files) (Char.escaped sq.[0]) in
        let j = index_of !(w.ranks) (Char.escaped sq.[1]) in
        let button = List.assoc (i, j) w.squares in
        GMisc.image ~pixbuf:(sprite 45 "dot") ~packing:button#set_image
          ()
        |> ignore;
        show_valid_moves_aux t
  in
  show_valid_moves_aux (valid_piece_moves !(w.board) p);
  ()

(** [from_square_callback w sq p] is the callback function for window
    [w] called when the mouse selects the from_square [sq] with the
    piece [p] on it for a move. *)
let from_square_callback w sq p =
  w.drop := true;
  w.from_square := Some sq;
  w.to_square := None;
  show_valid_moves w (extract p);
  ()

(** [to_square_callback w sq] is the callback function for window [w]
    called when the mouse selects the from_square [sq] for a move. *)
let to_square_callback w sq =
  let b = !(w.board) in
  let sq' = sq in
  let sq = extract !(w.from_square) in
  let p = extract (piece_of_square b sq) in
  let board' =
    if is_valid_move (sq, sq') b then
      if is_pawn_promotion b p sq' then (
        w.promotion := Some (p, sq');
        update_piece_select w;
        b )
      else move_piece b p sq' true
    else b
  in
  w.drop := false;
  w.from_square := None;
  w.to_square := None;
  w.board := board';
  update_window w;
  terminal_output board'

(** [game_pressed_callback w sq p] is the callback function for a button
    [sq] with piece option [p] pressed on window [w] in a game mode. *)
let game_pressed_callback w sq p =
  let valid_start =
    match p with
    | None -> false
    | Some p -> color_of_piece p = color_to_move !(w.board)
  in
  if (not !(w.drop)) || valid_start then
    if valid_start then from_square_callback w sq p else ()
  else to_square_callback w sq

(** [pressed_square_callback w btn] is the callback function for window
    [w] called when the mouse presses on a the button [btn]. *)
let pressed_square_callback w btn () =
  if !(w.locked) then ()
  else
    match !(w.promotion) with
    | Some _ -> ()
    | None ->
        let i, j = btn in
        let r = List.nth !(w.files) i in
        let c = List.nth !(w.ranks) j in
        let sq = r ^ c in
        let p = piece_of_square !(w.board) sq in
        game_pressed_callback w sq p

(** [gui_main ()] initiates the game in gui mode. *)
let gui_main mode elo fen =
  (* main game window *)
  let window =
    GWindow.window ~width ~height ~position:`CENTER ~resizable:true
      ~title:"OCaml Chess" ()
  in
  window#connect#destroy ==> Main.quit;
  window#misc#modify_bg bg_color;

  let rush =
    match mode with
    | SinglePlayer | TwoPlayer -> (None : rush option)
    | Rush -> Some (init_rush ())
  in
  let board =
    match mode with
    | SinglePlayer | TwoPlayer ->
        ref (try init_from_fen fen with Failure _ -> init_game ())
    | Rush -> ref (current_board (extract rush))
  in
  let elo =
    try elo |> int_of_string |> string_of_int with Failure _ -> "3000"
  in

  let computer =
    match mode with
    | SinglePlayer -> ref (Some Black)
    | TwoPlayer -> ref None
    | Rush -> ref (Some (computer_color (extract rush)))
  in

  let locked = ref false in
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
  let board_table =
    GPack.table ~packing:(add 0 0) ~homogeneous:true ()
  in
  let file_lbls, rank_lbls = add_file_rank_labels board_table in
  let squares = add_board_squares board board_table in

  let captured_table, black_captured, white_captured =
    init_captured_table (add 0 1)
  in

  let export_fen = GEdit.entry ~packing:(add 0 2) () in
  export_fen#set_text (export_to_fen !board);
  export_fen#set_editable false;

  let piece_select = init_piece_selection (add 1 0) in

  let puzzle_table, total_solved, total_wrong =
    init_puzzle_labels (add 0 3)
  in

  ( match mode with
  | SinglePlayer | TwoPlayer -> puzzle_table#misc#hide ()
  | Rush -> captured_table#misc#hide () );

  (* export_fen#misc#hide () ); *)
  (* TODO *)
  let game_window =
    {
      mode;
      elo;
      board;
      locked;
      computer;
      rush;
      drop;
      promotion;
      from_square;
      to_square;
      squares;
      captured_table;
      black_captured;
      white_captured;
      export_fen;
      files;
      ranks;
      file_lbls;
      rank_lbls;
      piece_select;
      total_solved;
      total_wrong;
    }
  in

  (* go back and set all the button callbacks *)
  let rec set_board_callbacks i j =
    if i < 8 then (
      if j = 8 then if i = 7 then () else set_board_callbacks (i + 1) 0
      else
        let button = List.assoc (i, j) squares in
        button#connect#enter ==> enter_square_callback game_window;
        button#connect#pressed
        ==> pressed_square_callback game_window (i, j);
        set_board_callbacks i (j + 1) )
  in
  set_board_callbacks 0 0;

  let rec set_piece_select_callbacks pts =
    match pts with
    | [] -> ()
    | h :: t ->
        let button = List.assoc h piece_select in
        button#connect#pressed ==> piece_select_callback game_window h;
        set_piece_select_callbacks t
  in
  set_piece_select_callbacks [ Rook; Bishop; Knight; Queen ];

  window#show ();
  Main.main ()

(** [main ()] initiates the game in gui mode. *)
let main =
  (* GtkMain.Main.init () |> ignore; *)
  let window =
    GWindow.window ~width:400 ~height:400 ~position:`CENTER
      ~resizable:true ~title:"OCaml Chess" ()
  in
  window#connect#destroy ==> Main.quit;
  window#misc#modify_bg bg_color;

  let table =
    GPack.table ~width:400 ~height:400 ~packing:window#add ()
  in
  let add i j x = table#attach i j x in

  let white_button = GButton.button ~packing:(add 1 0) () in
  white_button#misc#modify_bg light_color;
  white_button#set_border_width 5;
  text_label "One Player" 20 white_button#set_image |> ignore;

  let black_button = GButton.button ~packing:(add 2 0) () in
  black_button#misc#modify_bg light_color;
  black_button#set_border_width 5;
  text_label "Two Player" 20 black_button#set_image |> ignore;

  let rush_button = GButton.button ~packing:(add 1 2) () in
  rush_button#misc#modify_bg light_color;
  rush_button#set_border_width 5;
  text_label "Rush" 20 rush_button#set_image |> ignore;

  text_label "Elo: " 16 (add 0 3) |> ignore;
  let elo = GEdit.entry ~packing:(add 1 3) () in
  elo#set_text "900";

  text_label "Fen: " 16 (add 0 4) |> ignore;
  let fen = GEdit.entry ~packing:(add 1 4) () in
  fen#set_text "Paste an FEN here!";

  ( white_button#connect#pressed ==> fun () ->
    gui_main SinglePlayer elo#text fen#text );
  ( black_button#connect#pressed ==> fun () ->
    gui_main TwoPlayer elo#text fen#text );
  ( rush_button#connect#pressed ==> fun () ->
    gui_main Rush elo#text fen#text );

  window#show ();
  Main.main ()

let () = main
