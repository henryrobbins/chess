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
  en_passant : square option;
  ep_piece : p option;
}

let color_to_move t = t.color_to_move

let en_passant_sq t = t.en_passant

let en_passant_piece t = t.ep_piece

let can_castle t color side =
  match (color, side) with
  | White, King -> t.w_castle_ks
  | White, Queen -> t.w_castle_qs
  | Black, King -> t.b_castle_ks
  | Black, Queen -> t.b_castle_qs
  | _ -> failwith "impossible"

let active_pieces t = t.active_pieces

let captured_pieces t = t.captured_pieces

let piece_of_square t square = List.assoc square t.board

let id_of_piece p = p.id

let color_of_piece p = p.color

let square_of_piece p =
  match p.current_pos with
  | None -> failwith "[p] should not be captured"
  | Some sq -> sq

let square_of_king t c =
  let active_pieces = t.active_pieces in
  let king_matcher p =
    if p.color = c && p.id = King then true else false
  in
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

let get_en_passant sq sq' =
  let file = Char.escaped sq.[0] in
  let rank = int_of_string (Char.escaped sq.[1]) in
  let rank' = int_of_string (Char.escaped sq'.[1]) in
  if abs (rank - rank') = 2 then
    Some (file ^ string_of_int ((rank + rank') / 2))
  else None

let get_ep_piece active_pieces color_to_move ep_sq =
  match ep_sq with
  | None -> None
  | Some sq ->
      let file = Char.escaped sq.[0] in
      let rank = int_of_string (Char.escaped sq.[1]) in
      let color = switch_color color_to_move in
      let op = match color with White -> ( + ) | Black -> ( - ) in
      let rec search_pieces p_lst =
        match p_lst with
        | [] -> None
        | p :: t ->
            if
              color_of_piece p = color
              && square_of_piece p = file ^ string_of_int (op rank 1)
            then Some p
            else search_pieces t
      in
      search_pieces active_pieces

let extract_piece p_option : p =
  match p_option with None -> failwith "no piece" | Some p -> p

let move_piece t piece sq' =
  let state =
    match piece_of_square t sq' with
    | Some p -> capture_piece t p
    | None -> (
        match t.en_passant with
        | None -> t
        | Some ep_sq ->
            if piece.id = Pawn && sq' = ep_sq then
              capture_piece t (t.ep_piece |> extract_piece)
            else t )
  in
  let sq = square_of_piece piece in
  let piece' = { piece with current_pos = Some sq' } in
  let active =
    active_pieces state
    |> List.filter (fun x -> x <> piece)
    |> List.cons piece'
  in
  let board =
    state.board |> List.remove_assoc sq
    |> List.cons (sq, None)
    |> List.remove_assoc sq'
    |> List.cons (sq', Some piece')
  in
  let en_passant =
    match piece.id with Pawn -> get_en_passant sq sq' | _ -> None
  in
  let ep_piece =
    match en_passant with None -> None | Some _ -> Some piece'
  in
  {
    state with
    board;
    active_pieces = active;
    color_to_move = switch_color (color_of_piece piece);
    en_passant;
    ep_piece;
  }

let flip_turn t =
  { t with color_to_move = switch_color (color_to_move t) }

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
  match tup with
  | valid_fls, valid_rks ->
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
  List.(
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
    | _ -> [])

(** [l_it_from_sq sq] are the possible possible knight moves in
    clock-wise order starting from due N. Requires: [s] is in standard
    algebraic notation. *)
let l_it_from_sq sq =
  let rec gen_l_moves square list acc =
    match list with
    | [] -> List.rev acc
    | (d1, d2) :: t -> (
        let square1 = list_second (cardinal_it_from_sq square d1) in
        match square1 with
        | None -> gen_l_moves square t acc
        | Some s -> (
            let square2 = list_head (cardinal_it_from_sq s d2) in
            match square2 with
            | None -> gen_l_moves square t acc
            | Some s' -> gen_l_moves square t (s' :: acc) ) )
  in
  gen_l_moves sq
    [ (N, E); (E, N); (E, S); (S, E); (S, W); (W, S); (W, N); (N, W) ]
    []

let iterator_from_sq square direction : square list =
  match direction with
  | L -> l_it_from_sq square
  | _ -> cardinal_it_from_sq square direction

(** [piece_type_of_string s] is the piece type of the string id [s].
    Requires: [s] is in {P, R, B, N, Q, K} *)
let piece_type_of_string = function
  | "P" -> Pawn
  | "R" -> Rook
  | "B" -> Bishop
  | "N" -> Knight
  | "Q" -> Queen
  | "K" -> King
  | _ -> failwith "Invalid piece ID."

(** [color_of_string s] is the color of the string [s]. Requires: [s] is
    in {White, Black} *)
let color_of_string = function
  | "White" -> White
  | "Black" -> Black
  | _ -> failwith "Invalid piece color."

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
  let p_id =
    match String.lowercase_ascii id with
    | "k" -> King
    | "q" -> Queen
    | "b" -> Bishop
    | "n" -> Knight
    | "r" -> Rook
    | "p" -> Pawn
    | x -> failwith x
  in
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

let parse_color_to_play ctp =
  match ctp with
  | "w" -> White
  | "b" -> Black
  | _ -> failwith "not implemented"

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

let init_from_fen fen =
  let state_info_list = String.split_on_char ' ' fen in
  match state_info_list with
  | [ b; ctp; cast; ep; _; _ ] ->
      let en_passant = match ep with "-" -> None | x -> Some x in
      let color_to_move = parse_color_to_play ctp in
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
        en_passant;
        ep_piece = get_ep_piece active_pieces color_to_move en_passant;
      }
  | _ -> failwith "impossible"

let init_game () =
  init_from_fen
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(* [print_piece p] is a string with the color and id of piece [p]. If
   the piece is [None], it is a blank space. *)
let print_piece p =
  match p with
  | None -> "  "
  | Some p ->
      let c_map = [ (White, "W"); (Black, "B") ] in
      let id_map =
        [
          (Pawn, "P");
          (Rook, "R");
          (Bishop, "B");
          (Knight, "N");
          (Queen, "Q");
          (King, "K");
        ]
      in
      let c = List.assoc p.color c_map in
      let id = List.assoc p.id id_map in
      c ^ id

(** [print_board t] prints a graphical representation of the chess board
    where each square is labeled with the piece (if any) that is
    currently on that square. It also prints files and ranks along the
    side. *)
let print_board t =
  print_string "  -----------------------------------------\n";
  let row_str i =
    List.map (fun x -> x ^ i) files
    |> List.map (piece_of_square t)
    |> List.map print_piece
    |> List.fold_left (fun x y -> x ^ " | " ^ y) ""
  in
  List.iter
    (fun r -> print_string (r ^ row_str r ^ " |\n"))
    (List.rev ranks);
  print_string "  -----------------------------------------\n";
  print_string "    a    b    c    d    e    f    g    h\n"

let partition_pieces_by_color lst =
  let printer x = print_piece (Some x) in
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
      print_string ("\n" ^ "Black has Captured: ");
      print_string (string_of_string_list lst ^ "\n");
      print_string "White has Captured: ";
      print_string (string_of_string_list lst' ^ "\n")

let print_game_state t : unit =
  print_board t;
  print_captured t
