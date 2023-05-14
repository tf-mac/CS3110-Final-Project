open OUnit2
open RelationalDatabase.Cli

module type StringList = sig
  val input_list : string list ref
end

let better_print s = "'" ^ String.escaped s ^ "'"
let get_response = CLI.get_response

let handle_table_prints str =
  str |> String.split_on_char '\t' |> List.map String.trim
  |> List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) ""

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

let rec make_test primer tests is_print =
  match tests with
  | [] -> []
  | (reset, name, input, expected) :: t ->
      let placeholder =
        if reset then (
          CLI.reset ();
          add_input primer)
        else ();
        if is_print then
          cli_test name (CLI.parse_input input |> handle_table_prints) expected
        else cli_test name (CLI.parse_input input) expected
      in
      placeholder :: make_test primer t is_print

let fully_defined_generic =
  [
    "def Type ID";
    "int i";
    "float f";
    "char c";
    "bool b";
    "string s";
    "id d";
    "";
  ]

let fully_assigned_generic =
  fully_defined_generic
  @ [
      "assign Type ID1";
      "i = 10";
      "f = 3.14";
      "c = a";
      "b = true";
      "s = hello there";
      "";
    ]

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
      ( false,
        "no name error when blank spaces",
        "string   ",
        get_response "err_defn_no_name" );
      ( false,
        "cannot define the same type twice, end of defn",
        " ",
        get_response "indent_end" );
      ( false,
        "cannot define the same type twice, defn",
        "def Person Name",
        get_response "err_defn_already_exists" );
    ] )

let malformed_assign_states =
  ( [
      "def City Name";
      "int sq_footage";
      "float coordinates";
      "string nickname";
      "";
    ],
    [
      ( true,
        "assign with no extra terms",
        "assign ",
        get_response "err_assign_empty" );
      ( true,
        "assign with type and no id",
        "assign City",
        get_response "err_assign_no_id" );
      ( true,
        "assign with incorrect type and no id",
        "assign Coty",
        get_response "err_assign_no_id" );
      ( true,
        "assign with incorrect type and no id",
        "assign Coty Ithaca",
        get_response "err_assign_DNE" );
    ] )

let pre_defn_assign_tests =
  ( [ "def Person Name"; "int age"; "float bank"; ""; "assign Person John" ],
    [
      ( true,
        "can define types out of order bank",
        "bank = 5.4",
        get_response "indent" );
      ( false,
        "can define types out of order age",
        "age = 23",
        get_response "indent" );
      ( false,
        "cant define a type twice age",
        "age = 25",
        get_response "err_create_field_already_entered" );
      ( false,
        "can define types out of order empty end",
        "",
        get_response "indent_end" );
      ( true,
        "entry doesnt exist error",
        "test = 5",
        get_response "err_create_field_DNE" );
      ( true,
        "entry no value error",
        "age = ",
        get_response "err_create_field_no_value" );
      ( true,
        "assign with no name error",
        " = 5",
        get_response "err_create_field_no_value" );
      ( true,
        "entry wrong type error",
        "age = hello",
        get_response "err_create_field_wrong_type" );
    ] )

let assign_type_tests =
  ( fully_defined_generic @ [ "assign Type ID1" ],
    [
      ( true,
        "incorrect float value",
        "f = hello",
        get_response "err_create_field_wrong_type" );
      ( true,
        "incorrect int value",
        "i = 3.2",
        get_response "err_create_field_wrong_type" );
      ( true,
        "incorrect bool value",
        "b = 2",
        get_response "err_create_field_wrong_type" );
      ( true,
        "incorrect char value",
        "c = abc",
        get_response "err_create_field_wrong_type" );
    ] )

let find_tests =
  ( fully_assigned_generic,
    [
      ( true,
        "incorrect find comparison '_'",
        "find Type s _ 3",
        get_response "err_find_invalid_comparison" );
      ( true,
        "find type DNE",
        "find Tope i < 4",
        get_response "err_find_invalid_type" );
      ( true,
        "incorrect find expression",
        "find Type s _",
        get_response "err_find_invalid_expr" );
      ( true,
        "incorrect find expression, empty",
        "find Type s  ",
        get_response "err_find_invalid_expr" );
      ( true,
        "incorrect find comparison '+' with and",
        "find Type s > 4 and i = 2 and f + 3",
        get_response "err_find_invalid_comparison" );
      ( true,
        "incorrect find incorrect expr with and",
        "find Type s < 4 and i =2 and f <= 3",
        get_response "err_find_invalid_expr" );
      ( true,
        "empty find expression",
        "find ",
        get_response "err_find_missing_type" );
      ( true,
        "empty expr with and",
        "find Type  and   ",
        get_response "err_find_invalid_expr" );
    ] )

let misc_tests =
  ( [],
    [
      (true, "help message is correct", "help", get_response "help_message");
      ( true,
        "unknown command error on random input",
        "asdf",
        get_response "err_unknown_command" );
    ] )

let rec gather_tests_no_print tests =
  match tests with
  | [] -> []
  | (primer, tests) :: t ->
      make_test primer tests false @ gather_tests_no_print t

let gather_tests_with_print tests =
  match tests with
  | [] -> []
  | (primer, tests) :: t ->
      make_test primer tests true @ gather_tests_no_print t

let tests =
  ([
     def_tests;
     misc_tests;
     pre_defn_assign_tests;
     find_tests;
     assign_type_tests;
     malformed_assign_states;
   ]
  |> gather_tests_no_print)
  @ ([] |> gather_tests_with_print)

let suite = "search test suite" >::: tests
let _ = run_test_tt_main suite
