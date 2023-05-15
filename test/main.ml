open OUnit2
open RelationalDatabase.Tables
open RelationalDatabase.Database
open RelationalDatabase.Utils
open RelationalDatabase.Cli

let trim str =
  str |> String.split_on_char '\t' |> List.map String.trim
  |> List.filter (fun a -> a <> "")
  |> List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) ""
  |> String.trim

module TableTests (T : Table) = struct
  (*Table Tests*)

  let make_test name act exp = name >:: fun _ -> assert_equal exp act

  let person =
    T.empty
      [
        Type ("name", Strings);
        Type ("loc", Strings);
        Type ("age", Ints);
        Type ("net", Floats);
        Type ("alive", Bools);
        Type ("char", Chars);
        Type ("ids", Ids);
      ]

  let test_empty_table name ex expected =
    name >:: fun _ -> assert_equal expected (T.empty ex)

  let test_insert name table elist expected =
    name >:: fun _ ->
    assert_equal expected (T.insert table elist) ~printer:T.table_to_string

  let test_insert_exception name table elist expected =
    name >:: fun _ -> assert_raises expected (fun () -> T.insert table elist)

  let test_insert_named name table elist expected =
    name >:: fun _ -> assert_equal expected (T.insert_named table elist)

  let test_insert_named_exception name table elist expected =
    name >:: fun _ ->
    assert_raises expected (fun () -> T.insert_named table elist)

  let test_at name table id expected =
    name >:: fun _ -> assert_equal expected (T.at table id)

  let test_at_exception name table id expected =
    name >:: fun _ -> assert_raises expected (fun () -> T.at table id)

  let test_delete name table id expected =
    name >:: fun _ -> assert_equal expected (T.delete table id)

  let test_table_to_string name table expected =
    name >:: fun _ -> assert_equal expected (T.table_to_string table |> trim)

  let test_exists_exception name table table_name expected =
    name >:: fun _ ->
    assert_raises expected (fun () -> T.exists table table_name)

  let test_exists name table table_name expected =
    name >:: fun _ -> assert_equal expected (T.exists table table_name)

  let empty_table = T.empty []
  let table = T.empty [ Type ("name", Strings); Type ("age", Ints) ]

  let large_table =
    T.empty
      [
        Type ("String", Strings);
        Type ("Floats", Floats);
        Type ("Ints", Ints);
        Type ("Bools", Bools);
        Type ("Chars", Chars);
        Type ("Ids", Ids);
      ]

  let insert =
    T.insert large_table
      [
        String "John";
        Float 2.5;
        Int 9;
        Bool true;
        Char 'a';
        Id ("Person", String "Johnathan");
      ]

  let elist = [ ("name", String "John") ]
  let elist2 = [ ("age", Int 25) ]
  let elist_exception = [ ("", String "s") ]
  let elist_exception2 = [ ("name", String "John") ]
  let elist_for_at = [ Some (String "John") ]
  let insert_named = T.insert_named table elist
  let partial_table = T.empty [ Type ("name", Strings) ]
  let partial_insert = T.insert_named partial_table elist

  let table_tests =
    [
      test_empty_table "empty table test" [] empty_table;
      test_insert_named "add entry on existing table" table elist insert_named;
      test_insert_named_exception "TypeMismatch thrown on insert_named" table
        elist_exception TypeMismatch;
      test_insert_named_exception "IndexExists thrown on insert_named"
        insert_named elist_exception2 IndexExists;
      test_insert "testing insert" large_table
        [
          String "John";
          Float 2.5;
          Int 9;
          Bool true;
          Char 'a';
          Id ("Person", String "Johnathan");
        ]
        insert;
      test_insert_exception "IndexExists thrown on insert" insert_named
        [ String "John" ] IndexExists;
      test_at "testing at for simple table" partial_insert (String "John")
        elist_for_at;
      test_at_exception "testing invalid string for at" insert_named
        (String "invalid") Not_found;
      test_at_exception "testing invalid type for at" insert_named (Int 25)
        Not_found;
      test_at_exception "testing at for an empty table" empty_table
        (String "name") Not_found;
      test_delete "testing invalid delete" partial_table (String "invalid")
        partial_table;
      test_delete "testing empty delete" empty_table (String "invalid")
        empty_table;
      test_exists "testing exists on existing table" table "name" Strings;
      test_exists_exception "testing invalid exists on table" table "i"
        TypeMismatch;
      test_table_to_string "testing empty table to string" empty_table "";
      test_table_to_string "testing table to string" table "string name int age";
      make_test "No Strings insert"
        (List.nth
           ((T.insert_named person
               [
                 ("name", String "joe");
                 ("age", Int 5);
                 ("net", Float 5.);
                 ("alive", Bool true);
               ]
            |> T.at)
              (String "joe"))
           1)
        None;
      make_test "No Int insert"
        (List.nth
           ((T.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("net", Float 5.);
                 ("alive", Bool true);
               ]
            |> T.at)
              (String "joe"))
           2)
        None;
      make_test "No Floats insert"
        (List.nth
           ((T.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("alive", Bool true);
               ]
            |> T.at)
              (String "joe"))
           3)
        None;
      make_test "No Bools insert"
        (List.nth
           ((T.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("net", Float 5.);
               ]
            |> T.at)
              (String "joe"))
           4)
        None;
      make_test "No chars insert"
        (List.nth
           ((T.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("net", Float 5.);
               ]
            |> T.at)
              (String "joe"))
           5)
        None;
      make_test "No ids insert"
        (List.nth
           ((T.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("net", Float 5.);
               ]
            |> T.at)
              (String "joe"))
           6)
        None;
    ]
end

module ListTableTests = TableTests (ListTable)
module HashTableTests = TableTests (HashTable)

module DatabaseTests (T : Table) = struct
  module PersonDB = Database (T)

  let test_empty_database name expected =
    name >:: fun _ -> assert_equal expected PersonDB.empty

  let test_add_table fun_name database table name expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.add_table database table name) expected

  let test_add_table_exception fun_name database table name expected =
    fun_name >:: fun _ ->
    assert_raises expected (fun () -> PersonDB.add_table database table name)

  let test_build_table fun_name database table name expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.build_table database table name) expected

  let test_build_table_exception fun_name database table name expected =
    fun_name >:: fun _ ->
    assert_raises expected (fun () -> PersonDB.build_table database table name)

  let test_drop_table fun_name name database expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.drop_table name database) expected

  let test_get_table fun_name name database expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.get_table name database) expected

  let test_get_reference fun_name ent database expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.get_reference ent database) expected

  let test_add_entry fun_name table_name new_row database expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.add_entry table_name new_row database) expected

  let test_add_entry_exception fun_name table_name new_row database expected =
    fun_name >:: fun _ ->
    assert_raises expected (fun () ->
        PersonDB.add_entry table_name new_row database)

  let test_get_reference_exception fun_name ent database expected =
    fun_name >:: fun _ ->
    assert_raises expected (fun () -> PersonDB.get_reference ent database)

  let test_add_named_entry fun_name table_name new_row database expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.add_named_entry table_name new_row database) expected

  let test_add_named_entry_exception fun_name table_name new_row database
      expected =
    fun_name >:: fun _ ->
    assert_raises expected (fun () ->
        PersonDB.add_named_entry table_name new_row database)

  let test_database_to_string fun_name database expected =
    fun_name >:: fun _ ->
    assert_equal (PersonDB.db_to_string database |> trim) expected

  let database_person_ent_list = [ Type ("name", Strings); Type ("age", Ints) ]

  let database_airport_ent_list =
    [
      Type ("location", Strings);
      Type ("passengers", Strings);
      Type ("delayed", Bools);
    ]

  let t = T.empty [ Type ("name", Strings) ]
  let new_table = T.empty [ Type ("name", Strings); Type ("age", Ints) ]
  let elist = [ ("name", String "John"); ("age", Int 25) ]
  let add_table = T.insert_named new_table elist
  let reference = ([ String "name" ], Some [ Some (String "Person") ])

  let table2 =
    T.empty
      [
        Type ("location", Strings);
        Type ("passengers", Strings);
        Type ("delayed", Bools);
      ]

  let empty = PersonDB.empty
  let add_table_database = PersonDB.add_table empty add_table "Person"
  let person_database = PersonDB.add_table empty new_table "Person"
  let airport_database = PersonDB.add_table empty table2 "Airport"
  let large_database = PersonDB.add_table person_database table2 "Airport"
  let small_database = PersonDB.add_table empty t ""

  let database_tests =
    [
      test_empty_database "empty test" empty;
      test_add_table "Testing add_table" empty new_table "Person"
        person_database;
      test_add_table "Testing add_table to non-empty database" person_database
        table2 "Airport" large_database;
      test_add_table_exception
        "Raising failure by adding existing table to database" person_database
        new_table "Person" PersonDB.TableExists;
      test_drop_table "Testing drop_table" "Person" person_database empty;
      test_drop_table "Testing drop_table on empty table" "Person" empty empty;
      test_drop_table "Testing drop_table with invalid table" "Invalid"
        person_database person_database;
      test_drop_table "Testing drop_table on multi-table database" "Person"
        large_database airport_database;
      test_get_table "Testing Valid get_table" "Person" person_database
        (Some new_table);
      test_get_table "Testing Empty table for get_table" "Person" empty None;
      test_get_table "Testing Invalid table for get_table" "Airport"
        person_database None;
      test_get_table "Testing Valid get_table in multitable database" "Airport"
        large_database (Some table2);
      test_build_table "Testing Valid build_table on empty database" empty
        database_person_ent_list "Person" person_database;
      test_build_table "Testing Valid build_table on existing database"
        person_database database_airport_ent_list "Airport" large_database;
      test_build_table_exception "Testing TableExists exception on build_table"
        person_database database_person_ent_list "Person" PersonDB.TableExists;
      test_add_named_entry "Testing add_named_entry" "Person"
        [ ("name", String "location") ]
        person_database ();
      test_add_named_entry_exception "Testing Not_found in add_named_entry"
        "Invalid"
        [ ("name", String "location") ]
        person_database Not_found;
      test_database_to_string "Testing to string on empty database" empty "";
      test_database_to_string "Testing db_to_string" small_database
        "Table: \n\nstring name";
      test_get_reference "Test Get reference"
        (Id ("Person", String "John"))
        add_table_database
        ( [ Type ("name", Strings); Type ("age", Ints) ],
          Some [ Some (String "John"); Some (Int 25) ] );
      test_get_reference_exception "Testing Not Found for get reference"
        (Id ("Invalid", String "John"))
        add_table_database Not_found;
      test_get_reference "Testing Type Mismatch for get reference"
        (Id ("Person", String "Invalid"))
        add_table_database
        ([ Type ("name", Strings); Type ("age", Ints) ], None);
      test_add_entry "Testing add_entry" "Person" [ String "Amy"; Int 30 ]
        person_database ();
      test_add_entry_exception "Testing Not_found in add_entry" "invalid"
        [ String "Amy"; Int 30 ] person_database Not_found;
    ]
end

module DatabaseListTable = DatabaseTests (ListTable)
module DatabaseHashTable = DatabaseTests (HashTable)

module type StringList = sig
  val input_list : string list ref
end

let better_print s = "'" ^ String.escaped s ^ "'"
let get_response = CLI.get_response

let handle_table_prints str =
  str |> String.split_on_char '\t' |> List.map String.trim
  |> List.filter (fun a -> a <> "")
  |> List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) ""
  |> String.trim

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
  ( [
      "def Person Name";
      "int age";
      "float bank";
      "id friend";
      "";
      "assign Person John";
    ],
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
      ( false,
        "testing the id system doesnt throw errors 1",
        "assign Person Jim",
        get_response "indent" );
      ( false,
        "testing the id system throws error when no @",
        "friend = Person",
        get_response "err_create_field_wrong_type" );
      ( false,
        "testing the id system on multiple @",
        "friend = Person @ John @ Jim",
        get_response "err_create_field_wrong_type" );
      ( false,
        "testing the id system doesnt throw errors 2",
        "friend = Person @ John",
        get_response "indent" );
      ( false,
        "testing the id system doesnt throw errors 3",
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

let find_errors_tests =
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
      ( true,
        "find Type with wrong type in expr",
        "find Type i <= 3.2",
        get_response "err_find_wrong_type" );
      ( true,
        "find Type with wrong type in expr",
        "find Type k <= 3.2",
        get_response "err_find_var_DNE" );
    ] )

let find_tests =
  ( fully_assigned_generic
    @ [
        "assign Type ID2";
        "i = 1";
        "f = 2.71";
        "c = b";
        "b = false";
        "s = good bye";
        "";
      ],
    [
      ( true,
        "test find for no values with and",
        "find Type i = 1 and b = true",
        "string ID int i float f char c bool b string s id d" );
      ( true,
        "test find for one value with and",
        "find Type i = 1 and b = false",
        "string ID int i float f char c bool b string s id d ID2 1 2.71 b \
         false good bye" );
      ( true,
        "test find for two value with no and",
        "find Type i <> 5",
        "string ID int i float f char c bool b string s id d ID1 10 3.14 a \
         true hello there ID2 1 2.71 b false good bye" );
      ( true,
        "test find for one value with no and",
        "find Type i <= 1",
        "string ID int i float f char c bool b string s id d ID2 1 2.71 b \
         false good bye" );
      ( true,
        "test find for one value with no and",
        "find Type f >= 3",
        "string ID int i float f char c bool b string s id d ID1 10 3.14 a \
         true hello there" );
    ] )

let at_tests =
  ( fully_assigned_generic,
    [
      (true, "test empty at statement ", "at ", get_response "err_at_empty");
      (true, "test at with no id", "at Type", get_response "err_at_no_id");
      ( true,
        "test at statement with invalid id ",
        "at Type ID",
        get_response "err_at_id_DNE" );
      ( true,
        "test at statement with incorrect type name",
        "at Tupe ID",
        get_response "err_at_invalid_type" );
      ( true,
        "test at statement with non int column number",
        "at Type ID1 i",
        get_response "err_at_column_not_int" );
      ( true,
        "test at statement with out of range column number",
        "at Type ID1 10",
        get_response "err_at_column_out_of_range" );
      ( true,
        "test at statement on no entry column",
        "at Type ID1 6",
        get_response "no_entry" );
      ( true,
        "test at statement on column with int value 10 ",
        "at Type ID1 1",
        "10\n|> " );
      ( true,
        "test empty at statement ",
        "at Type ID 1",
        get_response "err_at_id_DNE" );
      ( true,
        "test empty at statement ",
        "at Tye ID 1",
        get_response "err_at_invalid_type" );
      ( true,
        "test empty at statement ",
        "assign Type ID2",
        get_response "indent" );
      ( false,
        "test empty at statement ",
        "d = Type @ asdf",
        get_response "indent" );
      (false, "test empty at statement ", "", get_response "indent_end");
      ( false,
        "test empty at statement ",
        "at Type ID2 6",
        "Type@asdf=\n\
         string ID\tint i\t\tfloat f\t\tchar c\t\tbool b\t\tstring s\tid d\t\t\n\n\
         <unbound val>\n\
         |> \n\
         |> " );
    ] )

let print_tests =
  ( fully_assigned_generic,
    [
      ( true,
        "test print on fully defined generic",
        "print",
        "Table: Type\n\n\
         string ID int i float f char c bool b string s id d ID1 10 3.14 a \
         true hello there |>" );
    ] )

let at_id_tests =
  ( fully_assigned_generic @ [ "assign Type ID2"; "d = Type @ ID1"; "" ],
    [
      ( true,
        "at tests on id instance",
        "at Type ID2 6",
        "Type@ID1=\n\
         string ID int i float f char c bool b string s id d ID1 10 3.14 a \
         true hello there |>" );
    ] )

let misc_tests =
  ( [],
    [
      (true, "help message is correct", "help", get_response "help_message");
      ( true,
        "unknown command error on random input",
        "asdf",
        get_response "err_unknown_command" );
      ( true,
        "unknown command error on typo input",
        "fi nd",
        get_response "err_unknown_command" );
    ] )

let rec gather_tests_no_print tests =
  match tests with
  | [] -> []
  | (primer, tests) :: t ->
      make_test primer tests false @ gather_tests_no_print t

let rec gather_tests_with_print tests =
  match tests with
  | [] -> []
  | (primer, tests) :: t ->
      make_test primer tests true @ gather_tests_with_print t

let tests =
  ([
     def_tests;
     misc_tests;
     pre_defn_assign_tests;
     find_errors_tests;
     assign_type_tests;
     malformed_assign_states;
     at_tests;
   ]
  |> gather_tests_no_print)
  @ ([ find_tests; print_tests; at_id_tests ] |> gather_tests_with_print)

let suite =
  "test suite"
  >::: List.flatten
         [
           ListTableTests.table_tests;
           HashTableTests.table_tests;
           DatabaseListTable.database_tests;
           DatabaseHashTable.database_tests;
           tests;
         ]

let _ = run_test_tt_main suite
