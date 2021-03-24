open Yojson.Basic.Util

type square = {
  pos : string}

type direction = N | NE | E | SE | S | SW | W | NW | L

type p = {
  id : string;
  color : string;
  positions : square list;
  number : string;
}

let ranks = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"]
let files = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]

type b = (square * p option) list

type t = {
  (**board : b;*)
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

let init_from_json board =  

let extract_positions (position1 : Yojson.Basic.t) : square =
  {
  pos = position1 |> member "pos" |> to_string;
  }
in
let extract_piece (piece1 : Yojson.Basic.t) : p =
  {
    id = piece1 |> member "id" |> to_string;
    color = piece1 |> member "color" |> to_string;
    positions = piece1 |> member "positions" |> to_list |> List.map extract_positions;
    number = piece1 |> member "number" |> to_string;
  }
in
let extract_board (board1 : Yojson.Basic.t)  =
  {
    active_pieces = board1 |> member "active_pieces" |> to_list |> List.map extract_piece;
    captured_pieces = board1 |> member "captured_pieces" |> to_list |> List.map extract_piece;
  }
in
extract_board 


let print t = failwith("Not implemented.")
