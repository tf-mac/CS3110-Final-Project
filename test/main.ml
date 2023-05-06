open OUnit2
open RelationalDatabase.Cli
module C = CLI

module type StringList = sig
  val input_list : string list
end

let better_print s = "'" ^ s ^ "'"

module TestAfterInput (C : CliHandler) (S : StringList) = struct
  let rec add_input lst =
    match lst with
    | [] -> ()
    | h :: t ->
        h |> C.parse_input |> ignore;
        add_input t

  let _ = add_input S.input_list
  let parse_input = C.parse_input
end

module DefinePersonType : StringList = struct
  let input_list = [ "def Person Name"; "int age"; "float bank_acc"; "" ]
end

module C1 = TestAfterInput (C) (DefinePersonType)

let _ = "print" |> C1.parse_input |> print_endline

let cli_test (name : string) (actual_output : string) (expected_output : string)
    : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output actual_output ~printer:better_print

(* let def_tests = [ cli_test "def test no error" "def a b" "|    " ]
   let suite = "search test suite" >::: List.flatten [ def_tests ]
   let _ = run_test_tt_main suite *)
