open Yojson.Basic.Util

type square = string

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
  id : string;
  color : string;
  current_pos : square option;
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

let square_of_piece p = p.current_pos

let id_of_piece p =
  match p with
  | None -> failwith "Piece must be non-None."
  | Some p -> p.id

let move_piece t piece s' =
  match piece with
  | None -> failwith "Requires [p] is a non-None piece option."
  | Some p ->
      let p' = { p with current_pos = Some s' } in
      let active =
        match piece_of_square t s' with
        | None ->
            active_pieces t
            |> List.filter (fun x -> x <> p)
            |> List.cons p'
        | Some cp ->
            active_pieces t
            |> List.filter (fun x -> x <> p)
            |> List.filter (fun x -> x <> cp)
            |> List.cons p'
            |> List.cons { cp with current_pos = None }
      in
      let captured =
        match piece_of_square t s' with
        | None -> captured_pieces t
        | Some cp -> { cp with current_pos = None } :: captured_pieces t
      in
      let board =
        match square_of_piece p with
        | None -> failwith "Piece should be active."
        | Some s ->
            t.board |> List.remove_assoc s
            |> List.cons (s, None)
            |> List.remove_assoc s'
            |> List.cons (s', Some p')
      in
      { board; active_pieces = active; captured_pieces = captured }

let color_of_piece p =
  match p with
  | None -> failwith "Piece must be non-None."
  | Some p -> p.color

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

let cardinal_it_from_sq square direction =
  List.(
    let fl = Char.escaped square.[0] in
    let rk = Char.escaped square.[1] in
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

let iterator_from_sq square direction =
  match direction with
  | L -> l_it_from_sq square
  | _ -> cardinal_it_from_sq square direction

(** [extract_active_piece j] extracts a list of active pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_active_piece j =
  let id = j |> member "id" |> to_string in
  let color = j |> member "color" |> to_string in
  let positions =
    j |> member "positions" |> to_list
    |> List.map (fun x -> to_string x)
  in
  let init_piece pos = { id; color; current_pos = Some pos } in
  List.map init_piece positions

(** [extract_captured_piece j] extracts a list of captured pieces from
    JSON. Requires: JSON is in valid format. *)
let extract_captured_piece j : p list =
  let id = j |> member "id" |> to_string in
  let color = j |> member "color" |> to_string in
  let n = j |> member "number" |> to_int in
  let captured_piece = { id; color; current_pos = None } in
  let rec piece_list pieces n =
    match n with
    | 0 -> pieces
    | n -> piece_list (captured_piece :: pieces) (n - 1)
  in
  piece_list [] n

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

(* [print_piece p] is a string with the color and id of piece [p]. If
   the piece is [None], it is a blank space. *)
let print_piece p =
  match p with
  | None -> "  "
  | Some p ->
      let c_map = [ ("White", "W"); ("Black", "B") ] in
      let c = List.assoc p.color c_map in
      c ^ p.id

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
      if p.color = "White" then (print_piece (Some p) :: acc1, acc2)
      else (acc1, print_piece (Some p) :: acc2)

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

let print t : unit =
  print_board t;
  print_captured t
