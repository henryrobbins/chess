open Yojson.Basic.Util

type square = string

type direction = N | NE | E | SE | S | SW | W | NW | L

type p = {
  id : string;
  color : string;
  current_pos : square option
}

let ranks = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"]
let files = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]

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
  | None -> failwith("Piece must be non-None.")
  | Some p -> p.id

let move_piece t piece s' =
  match piece with
  | None -> failwith("Requires [p] is a non-None piece option.")
  | Some p ->
    let p' = {p with current_pos = Some s'} in
    let active =
      match piece_of_square t s' with
      | None -> (active_pieces t)
                |> List.filter (fun x -> x <> p)
                |> List.cons p'
      | Some cp -> (active_pieces t)
                  |> List.filter (fun x -> x <> p)
                  |> List.filter (fun x -> x <> cp)
                  |> List.cons p'
                  |> List.cons {cp with current_pos = None} in
    let captured =
      match piece_of_square t s' with
      | None -> captured_pieces t
      | Some cp -> {cp with current_pos = None} :: captured_pieces t in
    let board =
      match square_of_piece p with
      | None -> failwith "Piece should be active."
      | Some s -> t.board
                  |> List.remove_assoc s
                  |> List.cons (s, None)
                  |> List.remove_assoc s'
                  |> List.cons (s', Some p') in
    {board=board; active_pieces=active; captured_pieces=captured}

let color_of_piece p =
  match p with
  | None -> failwith("Piece must be non-None.")
  | Some p -> p.color

let iterator_from_sq square direction = failwith("Not implemented.")

(** [extract_active_piece j] extracts a list of active pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_active_piece j =
  let id = j |> member "id" |> to_string in
  let color = j |> member "color" |> to_string in
  let positions = j |> member "positions"
                    |> to_list
                    |> List.map (fun x -> to_string x) in
  let init_piece pos = {id = id; color = color; current_pos = Some pos} in
  List.map init_piece positions

(** [extract_captured_piece j] extracts a list of captured pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_captured_piece j : p list =
  let id = j |> member "id" |> to_string in
  let color = j |> member "color" |> to_string in
  let n = j |> member "number" |> to_int in
  let captured_piece = {id = id; color = color; current_pos = None} in
  let rec piece_list pieces n =
    match n with
    | 0 -> pieces
    | n -> piece_list (captured_piece :: pieces) (n - 1) in
  piece_list [] n

(** [blank_board] is a blank chess board with no pieces on it. *)
let blank_board : b =
  let rec init rows cols b =
    match rows with
    | [] -> b
    | rh :: rt -> begin
      match cols with
      | [] -> init rt files b
      | ch :: ct -> init (rh :: rt) ct (((ch ^ rh),None) :: b)
    end in
  init ranks files []

let init_from_json j =
  let active_pieces = j |> member "active_pieces"
                        |> to_list
                        |> List.map extract_active_piece
                        |> List.flatten in
  let captured_pieces = j |> member "captured_pieces"
                          |> to_list
                          |> List.map extract_captured_piece
                          |> List.flatten in
  let rec add_pieces b piece_list =
    match piece_list with
    | [] -> b
    | h :: t -> let pos = h.current_pos in
      match pos with
      | None -> failwith("Active pieces should have a position.")
      | Some pos -> add_pieces ((pos, (Some h)) :: b) t in
  let board = add_pieces blank_board active_pieces in
  {
    board = board;
    active_pieces = active_pieces;
    captured_pieces = captured_pieces
  }

(* [print_piece p] is a string with the color and id of piece [p].
    If the piece is [None], it is a blank space. *)
let print_piece p =
  match p with
  | None -> "  "
  | Some p ->
    let c_map = [("White","W"); ("Black", "B")] in
    let c = List.assoc p.color c_map in
    c ^ p.id

(* TODO: Print our graveyard pieces somewhere. *)
let print t : unit =
  print_string "  -----------------------------------------\n";
  let row_str i = List.map (fun x -> x ^ i) files
                  |> List.map (piece_of_square t)
                  |> List.map print_piece
                  |> List.fold_left (fun x y -> x ^ " | " ^ y) ""
  in List.iter (fun r -> print_string (r ^ row_str r ^ " |\n")) (List.rev ranks);
  print_string "  -----------------------------------------\n";
  print_string "    a    b    c    d    e    f    g    h\n";