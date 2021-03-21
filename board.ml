type square = string

type direction = N | NE | E | SE | S | SW | W | NW | L

type p = {
  id : string;
  start_pos : square;
  color : string
}

(** Idea: record with field for each of the 64 squares of type p option. *)
type b

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

let init_from_json string = failwith("Not implemented.")

let print t = failwith("Not implemented.")
