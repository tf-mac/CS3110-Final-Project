open OUnit2
open RelationalDatabase.Cli

let better_print s = "'" ^ s ^ "'"

let cli_test (name : string) (input : string) (expected_output : string) : test
    =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (parse_input input) ~printer:better_print

let def_tests =
  [
    cli_test "def test no error" "def a b" "|    ";
    cli_test "def test no error" "def a b" "|    ";
  ]

let suite = "search test suite" >::: List.flatten [ def_tests ]
let _ = run_test_tt_main suite
