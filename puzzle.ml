open Board
open Engine
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

let empty_puz =
  {
    description = "empty puz";
    current_board =
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
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

(** [puzzle_move puz p m] is the next puzzle step in puzzle [puz], given
    that the user moved piece [p] to square [m]. If [m] was the optimal
    square to move to, [puz] advances to its next state, if there is
    one. *)
let puzzle_move puz p m =
  let t = init_from_fen (get_puz_current_board puz) in
  let next_player_square = move_piece t p m true in
  let next_player_fen = export_to_fen next_player_square in

  let best_move_fen =
    match get_player_moves puz with h :: t -> (h, t) | [] -> ("", [])
  in
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
      | [] -> failwith "reached the unreachable"
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
      current_board = next_player_fen;
      player_moves = get_player_moves puz;
      computer_moves = get_computer_moves puz;
      wrong = true;
      complete = false;
    }

(** [is_valid_puzzle_move puz fen] is the validity of the move from the next 
    valid player move in puzzle state [puz] to the game state represented by 
    [fen]. If the move is valid, then we will return a puzzle with the current
    board value updated to be the next computer move. Otherwise, we will update
    the wrong value to be true. *)

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
let get_next_puzzle rush = failwith "Unimplemented"

(** [init_rush rush] needs to make a list of random puzzles from the JSON file
    *)
let init_rush rush = failwith "Unimplemented"

let init_puz_from_fen initial p c = make_puz "A new puzzle" initial p c 

let play_puzzles rush = current_puz rush
