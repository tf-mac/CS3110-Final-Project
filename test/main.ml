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



let suite = "search test suite" >::: tests
let _ = run_test_tt_main suite
