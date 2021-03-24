open OUnit2
open Board


(* TODO: Write test cases for board *)
let board_tests = []

let suite =
  "test suite for chess"
  >::: List.flatten [ board_tests ]

let _ = run_test_tt_main suite
