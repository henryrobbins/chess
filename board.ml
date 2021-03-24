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

type b = (string * p option) list

type t = {
  board : b;
  active_pieces : p list;
  captured_pieces : p list;
}

let piece_of_square t square = failwith("Not implemented.")

let square_of_piece t p = failwith("Not implemented.")

let id_of_piece p = failwith("Not implemented.")

let start_sq_of_piece p = failwith("Not implemented.")

let color_of_piece p = failwith("Not implemented.")

let iterator_from_sq square direction = failwith("Not implemented.")

let active_pieces t = failwith("Not implemented.")

let captured_pieces t = failwith("Not implemented.")

let init_from_json t = failwith("Not implemented.")

let print t = failwith("Not implemented.")
