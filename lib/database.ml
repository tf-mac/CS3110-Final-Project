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

  let rec db_to_file database =
    match database with
    | [] -> String.make 1 (Char.chr 26)
    | (name, x) :: xs ->
        "Table=" ^ name ^ "\n" ^ Table.table_to_file !x ^ db_to_string xs

  let rec grab_name input ind =
    match input.[ind] with
    | '\n' ->
        ( String.sub input 0 ind,
          String.sub input ind (String.length input - ind) )
    | '\t' -> failwith "Grab name hit an entry!!!"
    | _ -> grab_name input (ind + 1)

  let rec find_end input ind newline =
    let eof = Char.chr 26 in
    match input.[ind] with
    | '\t' -> find_end input (ind + 1) false
    | e when e = eof -> ind
    | '\n' -> if newline then ind else find_end input (ind + 1) true
    | _ -> if newline then ind else find_end input (ind + 1) false

  let rec db_of_file_helper input acc =
    if String.sub input 0 7 <> "Table: " then acc
    else
      let name, ntbl =
        grab_name (String.sub input 7 (String.length input - 7)) 0
      in
      let end_of_table = find_end ntbl 0 false in
      db_of_file_helper
        (String.sub ntbl end_of_table (String.length ntbl - end_of_table))
        (acc
        @ [ (name, ref (Table.table_of_file (String.sub ntbl 0 end_of_table))) ]
        )

  let db_of_file input = db_of_file_helper input []

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
