open Utils
open Tables

module Database (Table : Table) = struct
  exception NoEntry
  exception WrongType
  exception TableExists

  type table = Table.t
  type database = (string * table ref) list

  let empty = []

  let add_table database table name : database =
    match
      List.find (fun a -> match a with tname, t -> name = tname) database
    with
    | exception Not_found -> (name, ref table) :: database
    | _ -> raise TableExists

  let build_table database table name =
    match
      List.find (fun a -> match a with tname, t -> name = tname) database
    with
    | exception Not_found -> (name, ref (Table.empty table)) :: database
    | _ -> raise TableExists

  let drop_table name database =
    List.filter (fun a -> match a with tname, t -> name <> tname) database

  let get_table name database =
    match List.assoc_opt name database with Some x -> Some !x | None -> None

  (*Currently doesn't work...*)
  let get_reference ent database =
    match ent with
    | Id (tbl, id) -> (
        let tbl =
          match get_table tbl database with
          | None -> raise Not_found
          | Some v -> v
        in
        ( Table.header tbl,
          match Table.at tbl id with exception Not_found -> None | v -> Some v
        ))
    | _ -> raise TypeMismatch

  let rec db_to_string database =
    match database with
    | [] -> "\n"
    | (name, x) :: xs ->
        "Table: " ^ name ^ "\n\n" ^ Table.table_to_string !x ^ db_to_string xs

  let rec add_entry table_name new_row database =
    match database with
    | [] -> raise Not_found
    | (a, b) :: tl when a = table_name -> b := Table.insert !b new_row
    | _ :: tl -> add_entry table_name new_row tl

  let add_named_entry table_name new_row database =
    match List.assoc_opt table_name database with
    | None -> raise Not_found
    | Some x -> x := Table.insert_named !x new_row
end
