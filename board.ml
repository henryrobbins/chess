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

let piece_of_square t square = failwith("Not implemented.")

let square_of_piece t p = failwith("Not implemented.")

let id_of_piece p = failwith("Not implemented.")

let move_piece t p s = failwith("Not implemented")

let start_sq_of_piece p = failwith("Not implemented.")

let color_of_piece p = failwith("Not implemented.")

let iterator_from_sq square direction = failwith("Not implemented.")

let active_pieces t = failwith("Not implemented.")

let captured_pieces t = failwith("Not implemented.")

(** [extract_active_piece j] extracts a list of active pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_active_piece j =
  let id = j |> member "id" |> to_string in
  let color = j |> member "color" |> to_string in
  let positions = j |> member "positions"
                    |> to_list
                    |> List.map (fun x -> to_string x) in
  let n = j |> member "number" |> to_int in
  let pieces = [] in
  pieces
  (* TODO: Define the active pieces. *)

(** [extract_captured_piece j] extracts aa list of captured pieces from JSON.
    Requires: JSON is in valid format. *)
let extract_captured_piece j : p list=
  let id = j |> member "id" |> to_string in
  let color = j |> member "color" |> to_string in
  let n = j |> member "number" |> to_int in
  let pieces = [] in
  pieces
  (* TODO: Define the captured pieces. *)

let init_from_json j =
  let active_pieces = j |> member "active_pieces"
                        |> to_list
                        |> List.map extract_active_piece in
  let captured_pieces = j |> member "captured_pieces"
                          |> to_list
                          |> List.map extract_captured_piece in
  (* TODO: Define the board. *)
  let board = empty in
  {
    board = board;
    (* Will need to make slight changes to this implementation. *)
    active_pieces = active_pieces;
    captured_pieces = captured_pieces
  }

let print t = failwith("Not implemented.")
