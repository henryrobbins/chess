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

type p = {
  id : piece_type;
  color : color;
  current_pos : square option;
  has_moved : bool;
}

let ranks = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8" ]

let files = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]

type b = (square * p option) list

type t = {
  board : b;
  active_pieces : p list;
  captured_pieces : p list;
}

let active_pieces t = t.active_pieces

let captured_pieces t = t.captured_pieces

let piece_of_square t square = List.assoc square t.board

let square_of_piece p =
  match p.current_pos with
  | None -> failwith "[p] should not be captured"
  | Some sq -> sq

let id_of_piece p = p.id

let has_moved p = p.has_moved

let color_of_piece p = p.color

let square_of_king c t =
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
  { board; active_pieces = active; captured_pieces = captured }

let move_piece t piece sq' =
  let state =
    match piece_of_square t sq' with
    | None -> t
    | Some p -> capture_piece t p
  in
  let sq = square_of_piece piece in
  let piece' =
    { piece with current_pos = Some sq'; has_moved = true }
  in
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
  { state with board; active_pieces = active }

let rec merge_singleton_and_list s lst acc reverse =
  if reverse then
    match lst with
    | [] -> List.rev acc
    | h :: t -> merge_singleton_and_list s t ((h ^ s) :: acc) true
  else
    match lst with
    | [] -> List.rev acc
    | h :: t -> merge_singleton_and_list s t ((s ^ h) :: acc) false

let rec zip_lists lst1 lst2 acc =
  match lst1 with
  | [] -> List.rev acc
  | h :: t -> (
      match lst2 with
      | [] -> List.rev acc
      | h' :: t' -> zip_lists t t' ((h ^ h') :: acc) )

let merge_rks_and_fls tup =
  match tup with
  | valid_fls, valid_rks ->
      if List.length valid_rks = 1 then
        match valid_rks with
        | h :: _ -> merge_singleton_and_list h valid_fls [] true
        | [] -> []
      else if List.length valid_fls = 1 then
        match valid_fls with
        | h :: _ -> merge_singleton_and_list h valid_rks [] false
        | [] -> []
      else zip_lists valid_fls valid_rks []

let candidate_lsts op1 op2 rk fl rev_rk rev_fl =
  let ord_ranks = if rev_rk then List.rev ranks else ranks in
  let ord_files = if rev_fl then List.rev files else files in
  let valid_rks = List.filter (op1 rk) ord_ranks in
  let valid_fls = List.filter (op2 fl) ord_files in
  (valid_fls, valid_rks)

let list_second list =
  match list with h :: h' :: _ -> Some h' | [ h ] -> None | [] -> None

let list_head list = match list with h :: _ -> Some h | [] -> None

let cardinal_it_from_sq (sq : square) direction : square list =
  List.(
    let fl = Char.escaped sq.[0] in
    let rk = Char.escaped sq.[1] in
    let iterator rk_op fl_op rev_rk rev_fl =
      let valid_rks_fls =
        candidate_lsts rk_op fl_op rk fl rev_rk rev_fl
      in
      merge_rks_and_fls valid_rks_fls
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

(* Returns possible knight moves in clock-wise order starting from due N*)
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

(** [extract_active_piece j] extracts a list of active pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_active_piece j =
  let id = j |> member "id" |> to_string |> piece_type_of_string in
  let color = j |> member "color" |> to_string |> color_of_string in
  let positions =
    j |> member "positions" |> to_list
    |> List.map (fun x -> to_string x)
  in
  let has_moved =
    j |> member "has_moved" |> to_list |> List.map to_bool
  in
  let fields_lst = List.combine positions has_moved in
  let init_piece = function
    | f1, f2 -> { id; color; current_pos = Some f1; has_moved = f2 }
  in
  List.map init_piece fields_lst

(** [extract_captured_piece j] extracts a list of captured pieces from
    JSON. Requires: JSON is in valid format. *)
let extract_captured_piece j : p list =
  let id = j |> member "id" |> to_string |> piece_type_of_string in
  let color = j |> member "color" |> to_string |> color_of_string in
  let has_moved =
    j |> member "has_moved" |> to_list |> List.map to_bool
  in
  let init_piece has_moved =
    { id; color; current_pos = None; has_moved }
  in
  List.map init_piece has_moved

(** [blank_board] is a blank chess board with no pieces on it. *)
let blank_board : b =
  let rec init rows cols b =
    match rows with
    | [] -> b
    | rh :: rt -> (
        match cols with
        | [] -> init rt files b
        | ch :: ct -> init (rh :: rt) ct ((ch ^ rh, None) :: b) )
  in
  init ranks files []

let init_from_json j =
  let active_pieces =
    j |> member "active_pieces" |> to_list
    |> List.map extract_active_piece
    |> List.flatten
  in
  let captured_pieces =
    j
    |> member "captured_pieces"
    |> to_list
    |> List.map extract_captured_piece
    |> List.flatten
  in
  let rec add_pieces b piece_list =
    match piece_list with
    | [] -> b
    | h :: t -> (
        let pos = h.current_pos in
        match pos with
        | None -> failwith "Active pieces should have a position."
        | Some pos -> add_pieces ((pos, Some h) :: b) t )
  in
  let board = add_pieces blank_board active_pieces in
  { board; active_pieces; captured_pieces }

let init_game () =
  "board_init.json" |> Yojson.Basic.from_file |> init_from_json

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

(* TODO: Print our graveyard pieces somewhere. *)
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

let rec partition_pieces_by_color lst acc1 acc2 =
  match lst with
  | [] -> (acc1, acc2)
  | p :: t ->
      if p.color = White then
        partition_pieces_by_color t (print_piece (Some p) :: acc1) acc2
      else
        partition_pieces_by_color t acc1 (print_piece (Some p) :: acc2)

let string_of_string_list lst =
  let rev_lst = List.rev lst in
  let rec build_str str lst' =
    match lst' with [] -> str | h :: t -> build_str (str ^ h ^ ", ") t
  in
  build_str "" rev_lst

let print_captured t =
  let print_lists = partition_pieces_by_color t.captured_pieces [] [] in
  match print_lists with
  | lst, lst' ->
      print_string ("\n" ^ "Black has Captured: ");
      print_string (string_of_string_list lst ^ "\n");
      print_string "White has Captured: ";
      print_string (string_of_string_list lst' ^ "\n")

let print_game_state t : unit =
  print_board t;
  print_captured t
