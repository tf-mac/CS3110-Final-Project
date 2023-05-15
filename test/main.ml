open OUnit2
open RelationalDatabase.Tables
open RelationalDatabase.Database
open RelationalDatabase.Utils

let trim str =
  str |> String.split_on_char '\t' |> List.map String.trim
  |> List.filter (fun a -> a <> "")
  |> List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) ""
  |> String.trim

module TableTests (T : Table) = struct
  (*Table Tests*)

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

let suite =
  "test suite"
  >::: List.flatten
         [
           ListTableTests.table_tests;
           HashTableTests.table_tests;
           DatabaseListTable.database_tests;
           DatabaseHashTable.database_tests;
         ]

let _ = run_test_tt_main suite
