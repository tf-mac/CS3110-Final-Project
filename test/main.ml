open OUnit2
open RelationalDatabase.Cli

module type StringList = sig
  val input_list : string list ref
end

let better_print s = "'" ^ String.escaped s ^ "'"
let get_response = CLI.get_response

let rec add_input lst =
  match lst with
  | [] -> ()
  | h :: t ->
      h |> CLI.parse_input |> ignore;
      add_input t

let cli_test (name : string) (actual_output : string) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output actual_output ~printer:better_print

let rec make_test primer tests =
  match tests with
  | [] -> []
  | (reset, name, input, expected) :: t ->
      let placeholder =
        if reset then (
          CLI.reset ();
          add_input primer)
        else ();
        cli_test name (CLI.parse_input input) expected
      in
      placeholder :: make_test primer t

let def_tests =
  ( [],
    [
      ( true,
        "def requires type name",
        "def ",
        get_response "err_defn_needs_type_name" );
      ( true,
        "def requires ID name",
        "def Person ",
        get_response "err_defn_needs_ID_name" );
      (true, "def test no error", "def Person Name", "|    ");
      ( false,
        "wrong type error",
        "flat float",
        get_response "err_defn_invalid_type" );
      (false, "wrong type error", "float   ", get_response "err_defn_no_name");
    ] )

let pre_defn_assign_tests =
  ( [ "def Person Name"; "int age"; "float bank"; ""; "assign Person John" ],
    [
      (true, "can define types out of order bank", "bank = 5.4", "|    ");
      (false, "can define types out of order age", "age = 23", "|    ");
      (false, "can define types out of order end", "", "|    <|\n|> ");
      ( true,
        "entry doesnt exist error",
        "test = 5",
        get_response "err_create_field_DNE" );
      ( true,
        "entry no value error",
        "age = ",
        get_response "err_create_field_no_value" );
      ( true,
        "entry wrong type error",
        "age = hello",
        get_response "err_create_field_wrong_type" );
    ] )

let misc_tests =
  ( [],
    [ (true, "help message is correct", "help", get_response "help_message") ]
  )

let rec gather_tests tests =
  match tests with
  | [] -> []
  | (primer, tests) :: t -> make_test primer tests :: gather_tests t

let suite =
  "search test suite"
  >::: ([ def_tests; misc_tests; pre_defn_assign_tests ]
       |> gather_tests |> List.flatten)

let _ = run_test_tt_main suite
