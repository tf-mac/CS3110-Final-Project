open OUnit2
open RelationalDatabase

let make_test name act exp = name >:: fun _ -> assert_equal exp act

module TableTests (Table : Tables.Table) = struct
  let person =
    Table.empty
      [
        Type ("name", Strings);
        Type ("loc", Strings);
        Type ("age", Ints);
        Type ("net", Floats);
        Type ("alive", Bools);
        Type ("char", Chars);
        Type ("ids", Ids);
      ]

  let ttests =
    [
      make_test "No Strings insert"
        (List.nth
           ((Table.insert_named person
               [
                 ("name", String "joe");
                 ("age", Int 5);
                 ("net", Float 5.);
                 ("alive", Bool true);
               ]
            |> Table.at)
              (String "joe"))
           1)
        None;
      make_test "No Int insert"
        (List.nth
           ((Table.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("net", Float 5.);
                 ("alive", Bool true);
               ]
            |> Table.at)
              (String "joe"))
           2)
        None;
      make_test "No Floats insert"
        (List.nth
           ((Table.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("alive", Bool true);
               ]
            |> Table.at)
              (String "joe"))
           3)
        None;
      make_test "No Bools insert"
        (List.nth
           ((Table.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("net", Float 5.);
               ]
            |> Table.at)
              (String "joe"))
           4)
        None;
      make_test "No chars insert"
        (List.nth
           ((Table.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("net", Float 5.);
               ]
            |> Table.at)
              (String "joe"))
           5)
        None;
      make_test "No ids insert"
        (List.nth
           ((Table.insert_named person
               [
                 ("name", String "joe");
                 ("loc", String "DC");
                 ("age", Int 5);
                 ("net", Float 5.);
               ]
            |> Table.at)
              (String "joe"))
           6)
        None;
    ]
end
