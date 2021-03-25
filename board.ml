open Yojson.Basic.Util
open Map

type square = string

type direction = N | NE | E | SE | S | SW | W | NW | L

type p = {
  id : string;
  color : string;
  current_pos : square option
}

let ranks = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"]
let files = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]

type b = (string , p option) Map.t

type t = {
  board : b;
  active_pieces : p list;
  captured_pieces : p list;
}

let piece_of_square t square = find square t.board

let square_of_piece t p = failwith("Not implemented.")

let id_of_piece p =
  match p with
  | None -> failwith("Piece must be non-None.")
  | Some p -> p.id

let move_piece t p s = failwith("Not implemented")

let start_sq_of_piece p = failwith("Not implemented.")

let color_of_piece p =
  match p with
    | None -> failwith("Piece must be non-None.")
    | Some p -> p.color

let iterator_from_sq square direction = failwith("Not implemented.")

let active_pieces t = t.active_pieces

let captured_pieces t = t.captured_pieces

(** [extract_active_piece j] extracts a list of active pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_active_piece j =
  (*  NOTE: We don't actually need the number field for active pieces.
      Should we remove that field or keep it for consistency? *)
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
          | ch :: ct -> init (rh :: rt) ct (insert (ch ^ rh) None b)
      end in
  init ranks files empty

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
          | Some pos -> add_pieces (insert pos (Some h) b) t in
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
      let c_map = insert "White" "W" (insert "Black" "B" empty) in
      let c = find p.color c_map in
      c ^ p.id

(* TODO: Print our graveyard pieces somewhere. *)
let print t : unit =
  print_string "    a    b    c    d    e    f    g    h\n";
  print_string "  -----------------------------------------\n";
  let row_str i = List.map (fun x -> x ^ i) files
                  |> List.map (piece_of_square t)
                  |> List.map print_piece
                  |> List.fold_left (fun x y -> x ^ " | " ^ y) ""
  in List.iter (fun r -> print_string (r ^ row_str r ^ " |\n")) ranks;
  print_string "  -----------------------------------------\n";