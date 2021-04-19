open OUnit2
open Board
open Command
open Validation
open Yojson.Basic.Util

let board = init_game ()

let extract j =
  let description = j |> member "description" |> to_string in
  let fen = j |> member "fen" |> to_string in
  (description, fen)

let init_tests_json json =
  json |> Yojson.Basic.from_file |> to_list |> List.map extract

let tests = init_tests_json "test_fens.json"

(* HELPER FUNCTIONS *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let string_of_string_tup_list tup_lst =
  let rev_lst = List.rev tup_lst in
  let rec build_str str lst' =
    match lst' with
    | [] -> str
    | (h, h') :: t ->
        build_str (str ^ "(" ^ h ^ ", " ^ h' ^ ")" ^ ", ") t
  in
  build_str "" rev_lst

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let print_tuple = function a, b -> "(" ^ a ^ ", " ^ b ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_pair pp1 pp2 (a, b)] pretty-prints [(a, b)] using [pp1] for [a]
    and [pp2] for [b]. *)
let pp_pair pp1 pp2 (a, b) = "(" ^ pp1 a ^ ", " ^ pp2 b ^ ")"

let pp_dir dir =
  match dir with
  | N -> "N"
  | S -> "S"
  | SW -> "SW"
  | SE -> "SE"
  | NW -> "NW"
  | NE -> "NE"
  | W -> "W"
  | E -> "E"
  | L -> "L"

let pp_dirs dirs =
  let rec string_maker dirs acc =
    match dirs with
    | [ h ] -> acc ^ pp_dir h ^ "]"
    | h :: t -> acc ^ pp_dir h ^ ";" ^ string_maker t acc
    | [] -> acc ^ "]"
  in
  "[" ^ string_maker dirs ""

let extract_piece piece_option =
  match piece_option with None -> failwith "no piece" | Some p -> p

let move b sq sq' =
  let p = piece_of_square b sq |> extract_piece in
  move_piece b p sq'

(** [move_piece_test name b s s'] constructs OUnit tests named [name]
    that assert [move_piece b (piece_of_square s) s'] is correct. *)
let move_piece_test name b s s' : test list =
  let p = piece_of_square b s |> extract_piece in
  let b' = move_piece b p s' in
  let p' = piece_of_square b' s' |> extract_piece in
  [
    ( name ^ " | previous board square" >:: fun _ ->
      assert_equal None (piece_of_square b' s) );
    ( name ^ " | new board square (piece id) " >:: fun _ ->
      assert_equal (id_of_piece p) (id_of_piece p') );
    ( name ^ " | new board square (piece color) " >:: fun _ ->
      assert_equal (color_of_piece p) (color_of_piece p') );
    ( name ^ " | new board square (piece pos) " >:: fun _ ->
      assert_equal (square_of_piece p') s' ~printer:(fun x -> x) );
  ]

(** [move_piece_special_moves_test name b p s expected_ep expected_cast]
    constructs a list of OUnit tests named [name] that assert the
    equality of [expected_ep] with the en-passant state in game state
    [b] and the boolean tuple [expected_cast] with the castle states in
    game state [b].*)
let move_piece_special_moves_test name b s s' expected_ep expected_cast
    =
  let b' = move b s s' in
  match expected_cast with
  | wk, wq, bk, bq ->
      [
        ( name ^ " | en_passant" >:: fun _ ->
          assert_equal expected_ep (en_passant_sq b') );
        ( name ^ " | white_kingside_castle" >:: fun _ ->
          assert_equal wk (can_castle b' White King) );
        ( name ^ " | white_queenside_castle" >:: fun _ ->
          assert_equal wq (can_castle b' White Queen) );
        ( name ^ " | black_kingside_castle" >:: fun _ ->
          assert_equal bk (can_castle b' Black King) );
        ( name ^ " | black_queenside_castle" >:: fun _ ->
          assert_equal bq (can_castle b' Black Queen) );
      ]

(** [iterator_from_sq_test name board s d expected] constructs an OUnit
    test named [name] that asserts the equality of [expected] with
    [iterator_from_sq s d]. *)
let iterator_from_sq_test name s d expected : test =
  name >:: fun _ ->
  assert_equal expected (iterator_from_sq s d) ~printer:(pp_list Fun.id)

let board_tests =
  [
    (* move_pieces tests *)
    (* TODO: add test cases for captured pieces. *)
    move_piece_test "d2 -> d4" board "d2" "d4";
    move_piece_test "e7 -> e5" board "e7" "e5";
    move_piece_test "g1 -> h3" board "g1" "h3";
    (* move_piece special moves tests *)
    move_piece_special_moves_test "d2 -> d4, d3 becomes en-passant"
      board "d2" "d4" (Some "d3")
      (true, true, true, true);
    (let board' = move board "e2" "e4" in
     let board'' = move board' "e7" "e6" in
     move_piece_special_moves_test "bong cloud can't castle" board''
       "e1" "e2" None
       (false, false, true, true));
    (* iterator_from_sq tests *)
    [
      iterator_from_sq_test "d5 -> N" "d5" N [ "d6"; "d7"; "d8" ];
      iterator_from_sq_test "d5 -> NE" "d5" NE [ "e6"; "f7"; "g8" ];
      iterator_from_sq_test "d5 -> E" "d5" E [ "e5"; "f5"; "g5"; "h5" ];
      iterator_from_sq_test "d5 -> SE" "d5" SE
        [ "e4"; "f3"; "g2"; "h1" ];
      iterator_from_sq_test "d5 -> S" "d5" S [ "d4"; "d3"; "d2"; "d1" ];
      iterator_from_sq_test "d5 -> SW" "d5" SW [ "c4"; "b3"; "a2" ];
      iterator_from_sq_test "d5 -> W" "d5" W [ "c5"; "b5"; "a5" ];
      iterator_from_sq_test "d5 -> NW" "d5" NW [ "c6"; "b7"; "a8" ];
      iterator_from_sq_test "d8 -> W" "d8" N [];
      iterator_from_sq_test "h8 -> NE" "h8" NE [];
      iterator_from_sq_test "h4 -> E" "h4" E [];
      iterator_from_sq_test "h1 -> SE" "h1" SE [];
      iterator_from_sq_test "d1 -> S" "d1" S [];
      iterator_from_sq_test "a1 -> SW" "a1" SW [];
      iterator_from_sq_test "a4 -> W" "a4" W [];
      iterator_from_sq_test "a8 -> N" "a8" NW [];
      iterator_from_sq_test "h7 -> NW" "h7" NW [ "g8" ];
      iterator_from_sq_test "d4 -> L" "d4" L
        [ "e6"; "f5"; "f3"; "e2"; "c2"; "b3"; "b5"; "c6" ];
      iterator_from_sq_test "a1 -> L" "a1" L [ "b3"; "c2" ];
      iterator_from_sq_test "b5 -> L" "b5" L
        [ "c7"; "d6"; "d4"; "c3"; "a3"; "a7" ];
    ];
  ]

(** [parse_test name str board expected] constructs an OUnit test named
    [name] that asserts the equality of [expected] with
    [parse str board]. *)
let parse_test name str board expected : test =
  name >:: fun _ -> assert_equal expected (parse str board)

(** [parse_invalid_squares_test name str board] constructs an OUnit test
    named [name] that asserts [parse str board] raises [InvalidSquares]. *)
let parse_invalid_squares_test name str board : test =
  name >:: fun _ ->
  let f () = parse str board in
  assert_raises InvalidSquares f

(** [parse_inconsistent_test name str board] constructs an OUnit test
    named [name] that asserts [parse str board] raises
    [InconsistentPlacement]. *)
let parse_inconsistent_test name str board : test =
  name >:: fun _ ->
  let f () = parse str board in
  assert_raises InconsistentPlacement f

(** [parse_malformed_test name str board] constructs an OUnit test named
    [name] that asserts [parse str board] raises [Malformed]. *)
let parse_malformed_test name str board : test =
  name >:: fun _ ->
  let f () = parse str board in
  assert_raises Malformed f

let command_tests =
  [
    parse_test "move P d2 to d3 -> Move [P; d2; to; d3]"
      "move P d2 to d3" board
      (Move [ "P"; "d2"; "to"; "d3" ]);
    parse_test "  move P   d2 to   d3 -> Move [P; d2; to; d3]"
      "  move P   d2 to   d3" board
      (Move [ "P"; "d2"; "to"; "d3" ]);
    parse_test "move N b1 to a3 -> Move [N; b1; to; a3]"
      "move N b1 to a3" board
      (Move [ "N"; "b1"; "to"; "a3" ]);
    parse_test "move P d7 to d5 -> Move [P; d7; to; d5]"
      "move P d7 to d5" board
      (Move [ "P"; "d7"; "to"; "d5" ]);
    parse_malformed_test "empty raises Malformed" "" board;
    parse_malformed_test "go P d2 to d3 raises Malformed"
      "go P d2 to d3" board;
    parse_malformed_test "move P d2 d3 raises Malformed" "move P d2 d3"
      board;
    parse_malformed_test "move W d2 to d3 raises Malformed"
      "move W d2 to d3" board;
    parse_malformed_test "move P d2 to d3 a raises Malformed"
      "move P d2 to d3 a" board;
    parse_invalid_squares_test "move P d2 to d9 raises InvalidSquares"
      "move P d2 to d9" board;
    parse_invalid_squares_test "move P d9 to d2 raises InvalidSquares"
      "move P d9 to d2" board;
    parse_invalid_squares_test "move P l2 to d4 raises InvalidSquares"
      "move P l2 to d4" board;
    parse_invalid_squares_test "move P d4 to l2 raises InvalidSquares"
      "move P d4 to l2" board;
    parse_inconsistent_test
      "move K d2 to d3 raises InconsistentPlacement" "move K d2 to d3"
      board;
    parse_inconsistent_test
      "move P a1 to a8 raises InconsistentPlacement" "move P a1 to a8"
      board;
  ]

(** [valid_moves_test name json expected] constructs an OUnit test named
    [name] that asserts the equality of the move list generated by
    [valid_moves color (init_from_json json)] and [expected]. *)
let valid_moves_test name json expected =
  name >:: fun _ ->
  let board = init_from_fen (List.assoc json tests) in
  let computed_moves = valid_moves board in
  assert_equal
    (List.filter (fun x -> not (List.mem x expected)) computed_moves)
    [] ~printer:string_of_string_tup_list;
  assert_equal
    (List.filter (fun x -> not (List.mem x computed_moves)) expected)
    [] ~printer:string_of_string_tup_list

(** [valid_piece_moves_test name json sq expected] constructs an OUnit
    test named [name] that asserts the equality of the move list
    generated by [valid_piece_moves board p] and [expected]. *)
let valid_piece_moves_test name json sq expected =
  name >:: fun _ ->
  let board = init_from_fen (List.assoc json tests) in
  let p =
    match piece_of_square board sq with
    | None -> failwith "bad test"
    | Some p' -> p'
  in
  let computed_moves = valid_piece_moves board p in
  assert_equal
    (List.filter (fun x -> not (List.mem x expected)) computed_moves)
    [] ~printer:string_of_string_tup_list;
  assert_equal
    (List.filter (fun x -> not (List.mem x computed_moves)) expected)
    [] ~printer:string_of_string_tup_list

let check_printer = function
  | NotCheck -> "Not Check"
  | Check dirs -> pp_dirs dirs

(** [is_check_test name json expected] constructs an OUnit test named
    [name] that asserts the equality of the check state generated by
    [is_check_test (init_from_json json)] and [expected]. *)
let is_check_test name json expected =
  name >:: fun _ ->
  let board = init_from_fen (List.assoc json tests) in
  let check_state = is_check board in
  assert_equal check_state expected ~printer:check_printer

let is_check_tests =
  [
    is_check_test "White has pinned piece and is in check from NW"
      "pinned_intercept"
      (Check [ NW ]);
    is_check_test "Black has pinned piece but is not in check"
      "blocked_black_unchecked" NotCheck;
    is_check_test "White in check from NE" "white_in_check_NE"
      (Check [ NE ]);
    is_check_test "White in check from N" "white_in_check_north_"
      (Check [ N ]);
    is_check_test "White in check from S" "white_in_check_S_"
      (Check [ S ]);
    is_check_test "White in check from SE" "white_in_check_SE"
      (Check [ SE ]);
    is_check_test "White in check from W" "white_in_check_W"
      (Check [ W ]);
    is_check_test "White in check from SW" "white_in_check_SW"
      (Check [ SW ]);
    is_check_test "White in stalemate" "white_in_stalemate" NotCheck;
    (* TODO: Check some black positions, as well as some L-shapes *)
    is_check_test "Black in checkmate" "must_block_check" (Check [ SE ]);
    is_check_test "Black in check N" "multiple_is_check_calls"
      (Check [ N ]);
    is_check_test "Black King in check L" "move_restricted_by_own_piece"
      (Check [ L ]);
    is_check_test "Black in check SW" "checked_by_king" (Check [ SW ]);
    is_check_test "Black in check East" "checkmate" (Check [ E ]);
    is_check_test "Neither color in check" "restricted_pawn_attack"
      NotCheck;
    is_check_test "Double check, L and SE, Queen" "double_check_SE"
      (Check [ SE; L ]);
    is_check_test "Double check, L and S, Queen" "dcheck_cardinal"
      (Check [ S; L ]);
    is_check_test "Double check, L and SW, Rook" "double_check_SW"
      (Check [ SW; L ]);
  ]

let validation_tests =
  [
    valid_moves_test "White moves in pinned/intercept position"
      "pinned_intercept"
      [ ("e1", "d1"); ("e1", "f1"); ("e1", "e2"); ("e1", "f2") ];
    valid_moves_test "Black moves to get out of check"
      "must_block_check" [ ("g7", "g6") ];
    valid_moves_test "Pinned black piece takes white piece."
      "capture_while_pinned"
      [
        ("a7", "a6");
        ("a7", "a5");
        ("b7", "b6");
        ("b7", "b5");
        ("c7", "c6");
        ("c7", "c5");
        ("d7", "d6");
        ("d7", "d5");
        ("e7", "e6");
        ("e7", "e5");
        ("h7", "h6");
        ("g6", "h5");
        ("g6", "h5");
        ("b8", "a6");
        ("b8", "c6");
        ("e8", "f7");
        ("f8", "g7");
        ("g8", "f6");
        ("g8", "h6");
        ("f8", "h6");
      ];
    valid_moves_test "Black king must capture or move to escape check."
      "king_moves_in_check"
      [
        ("g5", "f5");
        ("g5", "g6");
        ("g5", "h6");
        ("g5", "g4");
        ("g5", "h4");
      ];
    valid_moves_test
      "Prevent moves placing king under check by other king"
      "checked_by_king"
      [ ("b7", "a7"); ("b7", "c7") ];
    valid_moves_test "No valid moves when under checkmate" "checkmate"
      [];
    valid_moves_test "Capture a piece to prevent a check"
      "take_piece_to_stop_check"
      [ ("h8", "g8"); ("e7", "f8") ];
    valid_moves_test "Various pieces can intercept"
      "various_piece_intercepts"
      [
        ("b8", "c6");
        ("b8", "d7");
        ("c8", "d7");
        ("e8", "f7");
        ("d8", "d7");
        ("c7", "c6");
      ];
    valid_moves_test "Multiple queens pinned with restricted movement"
      "many_pinned_queens" [ ("h7", "f7") ];
    valid_moves_test "Pinned queen movement." "pinned_queen_movement"
      [ ("f7", "g6"); ("f7", "h5"); ("e8", "d8") ];
    valid_moves_test "Full range of pawn attack"
      "full_range_pawn_attack"
      [ ("f4", "g5"); ("f4", "f5"); ("f4", "e5") ];
    valid_moves_test "Pawn attack and initial one or two space move"
      "pawn_attack_and_2_spaces"
      [
        ("d2", "e3");
        ("d2", "d3");
        ("d2", "c3");
        ("d2", "d4");
        ("h1", "g2");
        ("h1", "h2");
      ];
    valid_moves_test "Pawn intercept from start square moving up 2."
      "pawn_2_space_intercept"
      [ ("d2", "d4"); ("a1", "a2") ];
    valid_moves_test "Test king having a move during a check"
      "king_move_during_check"
      [
        ("a8", "c6");
        ("b8", "d6");
        ("c8", "c6");
        ("c8", "e6");
        ("d8", "d6");
        ("d8", "f6");
        ("e8", "c6");
        ("e8", "e6");
        ("f8", "d6");
        ("f8", "f6");
        ("g8", "e6");
        ("h8", "f6");
        ("g6", "h7");
        ("g6", "g5");
        ("g6", "f5");
      ];
    valid_moves_test "Checkmate, enemy king can check a king"
      "check_from_king" [];
    valid_moves_test "Test multiple calls to is_check"
      "multiple_is_check_calls" [ ("e3", "f2") ];
    valid_moves_test "Move restricted by its own piece"
      "move_restricted_by_own_piece" [];
    valid_moves_test "Forced draw" "forced_draw" [];
    valid_moves_test "Restricted Pawn Attack" "restricted_pawn_attack"
      [
        ("d7", "c6");
        ("d7", "d6");
        ("d7", "e6");
        ("g8", "f8");
        ("g8", "g7");
        ("g8", "h7");
        ("g8", "h8");
      ];
    valid_moves_test "En-passant Discovered Checkmate"
      "en_passant_reverse_mate" [ ("f5", "g6") ];
    valid_piece_moves_test "King can not deliver a pin"
      "king_cant_deliver_pin" "e5"
      [ ("e5", "d4"); ("e5", "e4") ];
    valid_piece_moves_test "Pawn can not deliver a pin" "pawn_cant_pin"
      "f7"
      [ ("f7", "f6"); ("f7", "f5") ];
    valid_piece_moves_test "Take piece to intercept"
      "take_piece_to_intercept" "d3" [ ("d3", "e4") ];
  ]

let suite =
  "test suite for chess"
  >::: List.flatten
         [
           List.flatten board_tests;
           command_tests;
           validation_tests;
           is_check_tests;
         ]

let _ = run_test_tt_main suite
