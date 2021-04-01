open OUnit2
open Board
open Command
open Validation

let board = init_game ()

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

let extract_piece piece_option =
  match piece_option with None -> failwith "no piece" | Some p -> p

(** [move_piece_test name b s s'] constructs OUnit tests named [name]
    that assert [move_piece b (piece_of_square s) s'] is correct. *)
let move_piece_test name b s s' : test list =
  let p = piece_of_square b s |> extract_piece in
  let b' = move_piece b p s' in
  let p' = piece_of_square b' s' |> extract_piece in
  [
    ( name ^ " | previous board sqaure" >:: fun _ ->
      assert_equal None (piece_of_square b' s) );
    ( name ^ " | new board sqaure (piece id) " >:: fun _ ->
      assert_equal (id_of_piece p) (id_of_piece p') );
    ( name ^ " | new board sqaure (piece color) " >:: fun _ ->
      assert_equal (color_of_piece p) (color_of_piece p') );
    ( name ^ " | new board sqaure (piece has_moved) " >:: fun _ ->
      assert_equal true (has_moved p') ~printer:Bool.to_string );
    ( name ^ " | new board sqaure (piece pos) " >:: fun _ ->
      assert_equal (square_of_piece p') s' ~printer:(fun x -> x) );
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

(** [valid_moves_test name color json expected] constructs an OUnit test
    named [name] that asserts the equality of the move list generated by
    [valid_moves color (init_from_json json)] and [expected]. *)
let valid_moves_test name color json expected =
  name >:: fun _ ->
  let board = init_from_json json in
  let computed_moves = valid_moves color board in
  assert_equal
    (List.filter (fun x -> not (List.mem x computed_moves)) expected)
    [];
  assert_equal
    (List.filter (fun x -> not (List.mem x expected)) computed_moves)
    [] ~printer:string_of_string_tup_list

let validation_tests =
  [
    valid_moves_test "White moves in pinned/intercept position" White
      "pinned_intercept.json"
      [ ("e1", "d1"); ("e1", "f1"); ("e1", "e2"); ("e1", "f2") ];
    valid_moves_test "Black moves in pinned/intercept position" Black
      "pinned_intercept.json"
      [
        ("a1", "b1");
        ("a1", "c1");
        ("a1", "a2");
        ("a1", "a3");
        ("a1", "a4");
        ("a5", "a2");
        ("a5", "a3");
        ("a5", "a4");
        ("a5", "b4");
        ("a5", "c3");
        ("a5", "d2");
        ("a5", "e1");
        ("a5", "b5");
        ("a5", "c5");
        ("a5", "d5");
        ("a5", "e5");
        ("a5", "b6");
        ("a5", "c7");
        ("a5", "d8");
        ("a5", "f5");
        ("a5", "g5");
        ("a5", "h5");
        ("a5", "a6");
        ("a5", "a7");
        ("a5", "a8");
        ("b7", "a7");
        ("b7", "c7");
        ("b7", "b8");
        ("b7", "b6");
        ("b7", "a6");
        ("b7", "a8");
        ("b7", "c6");
        ("b7", "c8");
      ];
  ]

let suite =
  "test suite for chess"
  >::: List.flatten
         [ List.flatten board_tests; command_tests; validation_tests ]

let _ = run_test_tt_main suite
