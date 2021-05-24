open Yojson.Basic.Util

type square = string

type color =
  | White
  | Black

type piece_type =
  | Pawn
  | Rook
  | Bishop
  | Knight
  | Queen
  | King

type direction =
  | N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  | L

let ranks = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]

let files = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]

type p = {
  id : piece_type;
  color : color;
  current_pos : square option;
}

type b = (square * p option) list

type t = {
  board : b;
  active_pieces : p list;
  captured_pieces : p list;
  color_to_move : color;
  w_castle_ks : bool;
  w_castle_qs : bool;
  b_castle_ks : bool;
  b_castle_qs : bool;
  ep_sq : square option;
  ep_piece : p option;
  half_turns : int;
  full_turns : int;
}

(** [rev_map lst] is association list [lst] with keys and values
    reversed. *)
let rev_map lst =
  let rec rev_map acc lst =
    match lst with
    | [] -> acc
    | (a, b) :: t -> rev_map ((b, a) :: acc) t
  in
  rev_map [] lst

let color_map =
  [ ("White", White); ("Black", Black); ("w", White); ("b", Black) ]

let color_of_string s = List.assoc s color_map

let string_of_color c = List.assoc c (rev_map color_map)

let piece_id_map =
  [
    ("P", Pawn);
    ("R", Rook);
    ("B", Bishop);
    ("N", Knight);
    ("Q", Queen);
    ("K", King);
  ]

let piece_id_of_string s =
  try List.assoc s piece_id_map
  with Not_found -> failwith "invalid string identifier"

let string_of_piece_id p = List.assoc p (rev_map piece_id_map)

let half_turns t = t.half_turns

let string_of_piece p =
  match p with
  | None -> "  "
  | Some p ->
      let p_color = String.uppercase_ascii (string_of_color p.color) in
      p_color ^ string_of_piece_id p.id

let piece_value_map =
  [ (Pawn, 1); (Knight, 3); (Bishop, 3); (Rook, 5); (Queen, 9) ]

(** [value_of_piece p] is the value of the piece [p] *)
let value_of_piece p = List.assoc p.id piece_value_map

let value_of_captured t color =
  let l = List.filter (fun x -> x.color = color) t.captured_pieces in
  let sum_fun acc elt = acc + value_of_piece elt in
  List.fold_left sum_fun 0 l

let color_to_move t = t.color_to_move

let en_passant_sq t = t.ep_sq

let en_passant_piece t = t.ep_piece

let can_castle t color side =
  match (color, side) with
  | White, King -> t.w_castle_ks
  | White, Queen -> t.w_castle_qs
  | Black, King -> t.b_castle_ks
  | Black, Queen -> t.b_castle_qs
  | _ -> failwith "impossible" [@coverage off]

let active_pieces t = t.active_pieces

let captured_pieces t = t.captured_pieces

let piece_of_square t square = List.assoc square t.board

let id_of_piece p = p.id

let color_of_piece p = p.color

let square_of_piece p =
  match p.current_pos with
  | None -> failwith "[p] should not be captured" [@coverage off]
  | Some sq -> sq

let square_of_king t c =
  let active_pieces = t.active_pieces in
  let king_matcher p = p.color = c && p.id = King in
  let king = List.find king_matcher active_pieces in
  square_of_piece king

let capture_piece t piece =
  let sq = square_of_piece piece in
  let p' = { piece with current_pos = None } in
  let active = active_pieces t |> List.filter (fun x -> x <> piece) in
  let captured = p' :: captured_pieces t in
  let board = t.board |> List.remove_assoc sq |> List.cons (sq, None) in
  { t with board; active_pieces = active; captured_pieces = captured }

(** [switch_color c] returns the opposite of color [c]. *)
let switch_color = function White -> Black | Black -> White

(** [extract_piece p_opt] returns [p] where [p_opt] is [Some p].
    Requires: [p_opt] is not [None]. *)
let extract_piece p_opt : p =
  match p_opt with None -> failwith "no piece" | Some p -> p

(** [get_ep_sq sq sq'] returns [Some square] if the move from [sq] to
    [sq'] creates an en passant square [square]. Otherwise, returns
    [None]. *)
let get_ep_sq sq sq' =
  let file = Char.escaped sq.[0] in
  let rank = int_of_string (Char.escaped sq.[1]) in
  let rank' = int_of_string (Char.escaped sq'.[1]) in
  if abs (rank - rank') = 2 then
    Some (file ^ string_of_int ((rank + rank') / 2))
  else None

let search_for_ep_piece active_pieces sq color_to_move =
  let file = Char.escaped sq.[0] in
  let rank = int_of_string (Char.escaped sq.[1]) in
  let color = switch_color color_to_move in
  let op = match color with White -> ( + ) | Black -> ( - ) in
  let rec search_aux p_lst =
    match p_lst with
    | [] -> None
    | p :: t ->
        if
          color_of_piece p = color
          && square_of_piece p = file ^ string_of_int (op rank 1)
        then Some p
        else search_aux t
  in
  search_aux active_pieces

(** [get_ep_piece p_lst col ep_sq] is [Some piece] if [ep_sq] is a valid
    en-passant square and [piece] is the pawn that was moved to create
    it, otherwise [None] *)
let get_ep_piece active_pieces color_to_move ep_sq =
  match ep_sq with
  | None -> None
  | Some sq -> search_for_ep_piece active_pieces sq color_to_move

(** [is_castling_move piece sq'] returns true iff moving piece [piece]
    to square [sq'] is a castling move. *)
let is_castling_move piece sq' =
  let sq = square_of_piece piece in
  abs (int_of_char sq'.[0] - int_of_char sq.[0]) = 2

let promote_pawn t p id =
  let p' = { p with id } in
  let active =
    active_pieces t |> List.filter (fun x -> x <> p) |> List.cons p'
  in
  let board =
    match p.current_pos with
    | None -> t.board
    | Some sq ->
        t.board |> List.remove_assoc sq |> List.cons (sq, Some p')
  in
  { t with active_pieces = active; board }

let is_pawn_promotion t p sq' =
  match (p.color, p.id) with
  | White, Pawn -> sq'.[1] = '8'
  | Black, Pawn -> sq'.[1] = '1'
  | _ -> false

let update_active_pieces piece piece' t =
  let active_pieces' =
    t.active_pieces
    |> List.filter (fun x -> x <> piece)
    |> List.cons piece'
  in
  { t with active_pieces = active_pieces' }

let update_castles sq t =
  let update_castle color side =
    if can_castle t color side then
      match (sq, color, side) with
      | "e1", White, _ -> false
      | "a1", White, Queen -> false
      | "h1", White, King -> false
      | "e8", Black, King -> false
      | "a8", Black, Queen -> false
      | "h8", Black, King -> false
      | _ -> true
    else false
  in
  {
    t with
    w_castle_ks = update_castle White King;
    b_castle_ks = update_castle Black King;
    w_castle_qs = update_castle White Queen;
    b_castle_qs = update_castle Black Queen;
  }

let update_board sq sq' piece' t =
  let board' =
    t.board |> List.remove_assoc sq
    |> List.cons (sq, None)
    |> List.remove_assoc sq'
    |> List.cons (sq', Some piece')
  in
  { t with board = board' }

let update_color_to_move turn t =
  let color_to_move' =
    if turn then switch_color t.color_to_move else t.color_to_move
  in
  { t with color_to_move = color_to_move' }

let update_en_passant sq sq' piece' t =
  let ep_sq' =
    match piece'.id with Pawn -> get_ep_sq sq sq' | _ -> None
  in
  let ep_piece' =
    match ep_sq' with None -> None | Some _ -> Some piece'
  in
  { t with ep_sq = ep_sq'; ep_piece = ep_piece' }

let update_turn_counters turn reset col t =
  let full_turns' =
    if turn && col = Black then t.full_turns + 1 else t.full_turns
  in
  let half_turns' =
    if reset then 0 else if turn then 1 + t.half_turns else t.half_turns
  in
  { t with half_turns = half_turns'; full_turns = full_turns' }

let is_turn_reset piece t sq' =
  match (id_of_piece piece, piece_of_square t sq') with
  | Pawn, _ -> true
  | _, Some _ -> true
  | _ -> false

(** [capture_en_passant t p s] is the state [t] after piece [p] moves to
    square [s] in which a piece captured via en passant is now captured
    and no longer active. *)
let capture_en_passant t piece sq' =
  match t.ep_sq with
  | None -> t
  | Some ep_sq ->
      if piece.id = Pawn && sq' = ep_sq then
        capture_piece t (t.ep_piece |> extract_piece)
      else t

let rec move_piece t piece sq' turn =
  let reset_turn_count = is_turn_reset piece t sq' in
  let col = color_to_move t in
  let t_with_capture =
    match piece_of_square t sq' with
    | Some p -> capture_piece t p
    | None -> capture_en_passant t piece sq'
  in
  let sq = square_of_piece piece in
  let piece' = { piece with current_pos = Some sq' } in
  let out_state =
    t_with_capture
    |> update_board sq sq' piece'
    |> update_active_pieces piece piece'
    |> update_color_to_move turn
    |> update_castles sq
    |> update_en_passant sq sq' piece'
    |> update_turn_counters turn reset_turn_count col
  in
  castle_state_update out_state piece sq'

and move_rook_for_castle t piece sq' =
  if is_castling_move piece sq' then
    let move_rook r_sq r_sq' =
      move_piece t
        (r_sq |> piece_of_square t |> extract_piece)
        r_sq' false
    in
    match sq' with
    | "c8" -> move_rook "a8" "d8"
    | "g8" -> move_rook "h8" "f8"
    | "c1" -> move_rook "a1" "d1"
    | "g1" -> move_rook "h1" "f1"
    | _ -> failwith "impossible" [@coverage off]
  else t

(** [move_rook_for_castle t p s] is the state [t] where the rook has
    moved if the move of piece [p] to square [s] was a castle. Otherwise
    [t]. *)
and castle_state_update t piece sq' =
  match piece.id with
  | King -> move_rook_for_castle t piece sq'
  | _ -> t

(** [merge_singleton_and_list s lst rev] is the list of elements of list
    [lst] with the singleton [s] appended on. If [rev] is true, [s] is
    concatenated to back of elements of [lst]. Otherwise, [s] is
    concatenated to front of elements of [lst]. *)
let merge_singleton_and_list s lst reverse =
  let concat_s x = if reverse then x ^ s else s ^ x in
  List.map concat_s lst

(** [zip_lists lst1 lst2] is the same as List.combine [lst1] [lst2]
    except the two lists can have different lengths. The remaining
    elements of the longer list are ignored. *)
let zip_lists lst1 lst2 =
  let rec zip lst1 lst2 acc =
    match lst1 with
    | [] -> List.rev acc
    | h :: t -> (
        match lst2 with
        | [] -> List.rev acc
        | h' :: t' -> zip t t' ((h ^ h') :: acc) )
  in
  zip lst1 lst2 []

(** TODO: meaningful description of [merge_rks_and_fls tup dir]. *)
let merge_rks_and_fls tup dir =
  let valid_fls, valid_rks = tup in
  if List.mem dir [ E; W ] then
    match valid_rks with
    | h :: _ -> merge_singleton_and_list h valid_fls true
    | [] -> []
  else if List.mem dir [ N; S ] then
    match valid_fls with
    | h :: _ -> merge_singleton_and_list h valid_rks false
    | [] -> []
  else zip_lists valid_fls valid_rks

(** TODO: meaningful description of [candidate_lsts]. *)
let candidate_lsts op1 op2 rk fl rev_rk rev_fl =
  let ord_ranks = if rev_rk then List.rev ranks else ranks in
  let ord_files = if rev_fl then List.rev files else files in
  let valid_rks = List.filter (op1 rk) ord_ranks in
  let valid_fls = List.filter (op2 fl) ord_files in
  (valid_fls, valid_rks)

(** [list_head lst] is the head of the list [lst] if it exists. None
    otherwise. *)
let list_head list = match list with h :: _ -> Some h | [] -> None

(** [list_head lst] is the second element of the list [lst] if it
    exists. None otherwise. *)
let list_second list =
  match list with h :: h' :: _ -> Some h' | [ h ] -> None | [] -> None

let cardinal_it_from_sq (sq : square) direction : square list =
  let fl = Char.escaped sq.[0] in
  let rk = Char.escaped sq.[1] in
  let iterator rk_op fl_op rev_rk rev_fl =
    let valid_rks_fls =
      candidate_lsts rk_op fl_op rk fl rev_rk rev_fl
    in
    merge_rks_and_fls valid_rks_fls direction
  in
  match direction with
  | N -> iterator ( < ) ( = ) false false
  | NE -> iterator ( < ) ( < ) false false
  | E -> iterator ( = ) ( < ) false false
  | SE -> iterator ( > ) ( < ) true false
  | S -> iterator ( > ) ( = ) true false
  | SW -> iterator ( > ) ( > ) true true
  | W -> iterator ( = ) ( > ) false true
  | NW -> iterator ( < ) ( > ) false true
  | _ -> []

let l_move_in_dir_from_square sq d1 d2 =
  let square1 = list_second (cardinal_it_from_sq sq d1) in
  match square1 with
  | None -> []
  | Some s -> (
      let square2 = list_head (cardinal_it_from_sq s d2) in
      match square2 with None -> [] | Some s' -> [ s' ] )

(** [l_it_from_sq sq] are the possible possible knight moves in
    clock-wise order starting from due N. Requires: [s] is in standard
    algebraic notation. *)
let l_it_from_sq sq =
  let dir_tups =
    [ (N, E); (E, N); (E, S); (S, E); (S, W); (W, S); (W, N); (N, W) ]
  in
  let fold_fun acc (d1, d2) =
    acc @ l_move_in_dir_from_square sq d1 d2
  in
  List.fold_left fold_fun [] dir_tups

let iterator_from_sq square direction : square list =
  match direction with
  | L -> l_it_from_sq square
  | _ -> cardinal_it_from_sq square direction

let rec remove_first_n lst n =
  match lst with
  | [] -> []
  | h :: t -> if n > 0 then remove_first_n t (n - 1) else lst

let get_first_n lst n =
  let rec get_n_aux lst' n' acc =
    match lst' with
    | [] -> acc
    | h :: t -> if n > 0 then get_n_aux t (n' - 1) (h :: acc) else acc
  in
  get_n_aux lst n []

let rec gen_empty_squares rk fls acc =
  match fls with
  | [] -> acc
  | h :: t -> gen_empty_squares rk t ((h ^ rk, None) :: acc)

let gen_piece id rk fl : p =
  let color = if String.uppercase_ascii id = id then White else Black in
  let p_id = piece_id_of_string (String.uppercase_ascii id) in
  let sq = fl ^ rk in
  { id = p_id; color; current_pos = Some sq }

let extract_rank rank_str rk : (square * p option) list =
  let rec extract_aux i fls acc =
    match fls with
    | [] -> acc
    | h :: t -> (
        match rank_str.[i] with
        | x -> (
            try
              let skip = int_of_string (Char.escaped x) in
              let unread_fls = remove_first_n fls skip in
              let skip_fls = get_first_n fls skip in
              extract_aux (i + 1) unread_fls
                (acc @ gen_empty_squares rk skip_fls [])
            with exn ->
              let piece = gen_piece (Char.escaped x) rk h in
              extract_aux (i + 1) t ((h ^ rk, Some piece) :: acc) ) )
  in
  extract_aux 0 files []

let extract_board board_str =
  let by_rank = String.split_on_char '/' board_str in
  let rks = List.rev ranks in
  let rec extract i acc =
    if i >= List.length by_rank then acc
    else
      let rk_str = List.nth by_rank i in
      let rk = List.nth rks i in
      extract (i + 1) acc @ extract_rank rk_str rk
  in
  extract 0 []

let active_pieces_of_board b_assoc =
  let rec a_piece_aux board a_pieces =
    match board with
    | [] -> a_pieces
    | (sq, p) :: t -> (
        match p with
        | None -> a_piece_aux t a_pieces
        | Some p' -> a_piece_aux t (p' :: a_pieces) )
  in
  a_piece_aux b_assoc []

let init_helper b ctp cast ep ht ft =
  let ep_sq = match ep with "-" -> None | x -> Some x in
  let color_to_move = color_of_string ctp in
  let active_pieces = active_pieces_of_board (extract_board b) in
  {
    board = extract_board b;
    captured_pieces = [];
    active_pieces;
    color_to_move;
    w_castle_ks = String.contains cast 'K';
    w_castle_qs = String.contains cast 'Q';
    b_castle_ks = String.contains cast 'k';
    b_castle_qs = String.contains cast 'q';
    ep_sq;
    ep_piece = get_ep_piece active_pieces color_to_move ep_sq;
    half_turns = int_of_string ht;
    full_turns = int_of_string ft;
  }

let init_from_fen fen =
  let state_info_list = String.split_on_char ' ' fen in
  match state_info_list with
  | [ b; ctp; cast; ep; ht; ft ] -> init_helper b ctp cast ep ht ft
  | _ -> failwith "impossible" [@coverage off]

let piece_identifier p =
  let p_id = string_of_piece_id p.id in
  match p.color with
  | White -> String.uppercase_ascii p_id
  | Black -> String.lowercase_ascii p_id

let id_to_prepend acc id =
  if acc > 0 then string_of_int acc ^ id else id

(** [board_fen_string t] is the component of the FEN string representing
    the current pieces on the board [t.board]. *)
let board_fen_string b =
  let rank_to_fen rank =
    let rec it_files files acc =
      match files with
      | [] -> if acc > 0 then string_of_int acc else ""
      | h :: t -> (
          match piece_of_square b (h ^ rank) with
          | None -> it_files t (acc + 1)
          | Some p ->
              let id = piece_identifier p in
              id_to_prepend acc id ^ it_files t 0 )
    in
    it_files files 0
  in
  ranks |> List.rev |> List.map rank_to_fen |> String.concat "/"

(** [next_to_move_string t] is the string representation of the player
    whose turn it is: "Black" or "White". *)
let next_to_move_string t =
  match t.color_to_move with Black -> "b" | White -> "w"

(** [char_to_castle lst] is the string representation of the list [lst]
    form of possible castles. *)
let char_to_castle lst =
  let sorted = List.sort Stdlib.compare lst in
  if sorted = [] then "-"
  else
    let rec concat lst acc =
      match lst with
      | [ h ] -> acc
      | h :: t -> concat lst (h ^ acc)
      | _ -> failwith "impossible" [@coverage off]
    in
    concat sorted ""

(** [castle_fen_string t] is the component of the FEN string
    representing the possible castles in the board state [t]. If no
    castles possible, '-'. *)
let castle_fen_string t =
  let bq_str = if t.b_castle_qs then "q" else "" in
  let bk_str = if t.b_castle_ks then "k" else "" in
  let wq_str = if t.w_castle_qs then "Q" else "" in
  let wk_str = if t.w_castle_ks then "K" else "" in
  let castle_str = wk_str ^ wq_str ^ bk_str ^ bq_str in
  if castle_str = "" then "-" else castle_str

let en_passant_string t =
  match en_passant_sq t with None -> "-" | Some sq -> sq

let export_to_fen t =
  let board_str = board_fen_string t in
  let turn_str = next_to_move_string t in
  let castle_str = castle_fen_string t in
  let en_passant_str = en_passant_string t in
  board_str ^ " " ^ turn_str ^ " " ^ castle_str ^ " " ^ en_passant_str
  ^ " "
  ^ string_of_int t.half_turns
  ^ " "
  ^ string_of_int t.full_turns

let init_game () =
  init_from_fen
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(** [print_board t] prints a graphical representation of the chess board
    where each square is labeled with the piece (if any) that is
    currently on that square. It also prints files and ranks along the
    side. *)
let print_board t =
  print_string "  -----------------------------------------\n";
  let row_str i =
    List.map (fun x -> x ^ i) files
    |> List.map (piece_of_square t)
    |> List.map string_of_piece
    |> List.fold_left (fun x y -> x ^ " | " ^ y) ""
  in
  List.iter
    (fun r -> print_string (r ^ row_str r ^ " |\n"))
    (List.rev ranks);
  print_string "  -----------------------------------------\n";
  print_string "    a    b    c    d    e    f    g    h\n"
  [@@coverage off]

let partition_pieces_by_color lst =
  let printer x = string_of_piece (Some x) in
  let white =
    List.filter (fun x -> x.color = White) lst |> List.map printer
  in
  let black =
    List.filter (fun x -> x.color = Black) lst |> List.map printer
  in
  (white, black)

let string_of_string_list lst =
  let rev_lst = List.rev lst in
  let rec build_str str lst' =
    match lst' with [] -> str | h :: t -> build_str (str ^ h ^ ", ") t
  in
  build_str "" rev_lst

let print_captured t =
  let print_lists = partition_pieces_by_color t.captured_pieces in
  match print_lists with
  | lst, lst' ->
      let b_score = value_of_captured t Black |> string_of_int in
      let w_score = value_of_captured t White |> string_of_int in
      print_string ("White has Captured (" ^ b_score ^ "): ");
      print_string (string_of_string_list lst' ^ "\n");
      print_string ("\n" ^ "Black has Captured (" ^ w_score ^ "): ");
      print_string (string_of_string_list lst ^ "\n")
  [@@coverage off]

let print_game_state t : unit =
  print_board t;
  print_captured t
  [@@coverage off]
