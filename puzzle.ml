open Board
open Engine
open Random
open Yojson.Basic.Util

type fen = string

type move = square * piece_type option

type puz = {
  description : string;
  current_board : fen;
  player_moves : fen list;
  computer_moves : fen list;
  wrong : bool;
  complete : bool;
}

type rush = {
  remaining : puz list;
  current_puz : puz;
  solved : int;
  total_wrong : int;
}

let extract_puz j =
  let description = j |> member "description" |> to_string in
  let current_board = j |> member "current_board" |> to_string in
  let player_moves =
    j |> member "player_moves" |> to_list |> List.map to_string
  in
  let computer_moves =
    j |> member "computer_moves" |> to_list |> List.map to_string
  in
  let wrong = j |> member "wrong" |> to_string |> bool_of_string in
  let complete =
    j |> member "complete" |> to_string |> bool_of_string
  in 
  { description; current_board; player_moves; computer_moves; wrong; complete } 

let puzzles =  
  "puzzles.json" |> Yojson.Basic.from_file |> to_list |> List.map extract_puz

let init_chessboard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let empty_puz =
  {
    description = "empty puz";
    current_board = init_chessboard;
    player_moves = [];
    computer_moves = [];
    wrong = false;
    complete = true;
  }

let empty_rush = {
  remaining = [];
  current_puz = empty_puz;
  solved = 0;
  total_wrong = 0;
}

let get_puz_description puz = puz.description

let get_puz_current_board puz = puz.current_board

let get_player_moves puz = puz.player_moves

let get_computer_moves puz = puz.computer_moves

let get_wrong puz = puz.wrong

let get_complete puz = puz.complete

let get_remaining rush = rush.remaining

let current_puz rush = rush.current_puz

let solved_rush rush = rush.solved

let wrong_rush rush = rush.total_wrong

let puzzle_move puz fen =
  let next_player_fen = fen in
  let best_move_fen = match get_player_moves puz with 
  | [] -> print_string "Puzzle completed!"; ("", [])
  | h :: t -> h, t in
  if
    next_player_fen = (best_move_fen |> fst)
    && get_computer_moves puz = []
  then
    {
      description = get_puz_description puz;
      current_board = next_player_fen;
      player_moves = best_move_fen |> snd;
      computer_moves = [];
      wrong = false;
      complete = true;
    }
  else if
    next_player_fen = (best_move_fen |> fst)
    && get_computer_moves puz != []
  then
    let remaining_comp_moves =
      match get_computer_moves puz with
      | h :: t -> (h, t)
      | [] -> print_string "reached the unreachable"; (init_chessboard, [])
    in
    {
      description = get_puz_description puz;
      current_board = remaining_comp_moves |> fst;
      player_moves = best_move_fen |> snd;
      computer_moves = remaining_comp_moves |> snd;
      wrong = false;
      complete = false;
    }
  else
    {
      description = get_puz_description puz;
      current_board = get_puz_current_board puz;
      player_moves = get_player_moves puz;
      computer_moves = get_computer_moves puz;
      wrong = true;
      complete = false;
    }

let solve_puzzle rush =
  match get_remaining rush with
  | h :: t -> (h, t)
  | [] -> (empty_puz, [])

let next_puz_from_rush rush =
  let puzzle_new = match get_remaining rush with 
  | [] -> failwith "Impossible" 
  | h :: t -> h in
  match (get_computer_moves puzzle_new, get_wrong puzzle_new) with
  | [], false ->
      {
        remaining = solve_puzzle rush |> snd;
        current_puz = solve_puzzle rush |> fst;
        solved = solved_rush rush + 1;
        total_wrong = wrong_rush rush;
      }
  | _, true ->
      {
        remaining = solve_puzzle rush |> snd;
        current_puz = solve_puzzle rush |> fst;
        solved = solved_rush rush;
        total_wrong = wrong_rush rush + 1;
      }
  | _ ->
      {
        remaining = get_remaining rush;
        current_puz = puzzle_new;
        solved = solved_rush rush;
        total_wrong = wrong_rush rush;
      }

let make_puz descrip current player computer = 
  {
    description = descrip;
    current_board = current;
    player_moves = player;
    computer_moves = computer;
    wrong = false;
    complete = false;
  }

let make_rush puz_list init = {
  remaining = puz_list;
  current_puz = init;
  solved = 0;
  total_wrong = 0;  
}

(** [get_next_puzzle rush] takes the next puzzle from the remaining puzzles. *)
let get_next_puzzle rush = match get_remaining rush with 
| h :: t -> h 
| [] -> print_string "This puzzle has been completed!"; empty_puz

(** [init_rush] needs to make a list of random puzzles from the JSON file
    *)
let init_rush () = 
  let rec helper acc counter = 
    let index = Random.int (List.length puzzles) in
    match counter with 
    | 0 -> acc
    | _ -> helper (List.nth puzzles index :: acc) (counter - 1)
  in helper [] 20

let init_puz_from_fen initial p c = make_puz "A new puzzle" initial p c 

let play_puzzles rush = current_puz rush
