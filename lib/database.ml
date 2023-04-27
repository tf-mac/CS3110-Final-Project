open Utils
open Tables

module Database (Table : Table) = struct
  exception NoEntry
  exception WrongType
  exception TableExists

  type table = Table.t
  type database = (string * table) list

  let empty = []

  let add_table database table name : database =
    match
      List.find (fun a -> match a with tname, t -> name = tname) database
    with
    | exception Not_found -> (name, table) :: database
    | _ -> raise TableExists

  let build_table database table name =
    match
      List.find (fun a -> match a with tname, t -> name = tname) database
    with
    | exception Not_found -> (name, Table.empty table) :: database
    | _ -> raise TableExists

  let drop_table name database =
    List.filter (fun a -> match a with tname, t -> name <> tname) database

  let get_table name database =
    match
      List.find (fun a -> match a with tname, t -> name = tname) database
    with
    | exception Not_found -> None
    | a, b -> Some b

  (*Currently doesn't work...*)
  let get_reference ent database = raise (Failure "Unimplemented")

  let rec db_to_string database =
    match database with
    | [] -> "\n"
    | (name, x) :: xs ->
        "Table:\t" ^ name ^ "\n" ^ Table.table_to_string x ^ db_to_string xs

  let check_value database tn eid ev =
    match List.find (fun a -> match a with n, tt -> n = tn) database with
    | x -> (
        match x with
        | _, [] -> raise Stack_overflow
        | _, types :: rest -> (
            match
              List.find
                (fun a -> match a with Type (l, ty) -> l = eid | _ -> false)
                types
            with
            | exception Not_found -> raise NoEntry
            | Type (label, label_type) -> (
                match label_type with
                | Ints -> (
                    match int_of_string_opt ev with
                    | None -> raise WrongType
                    | Some _ -> ())
                | Floats -> (
                    match float_of_string_opt ev with
                    | None -> raise WrongType
                    | Some _ -> ())
                | Strings -> ()
                | Chars -> ()
                | Bools -> (
                    match bool_of_string_opt ev with
                    | None -> raise WrongType
                    | Some _ -> ())
                | _ -> ())
            | _ -> raise Stack_overflow))
    | exception Not_found -> print_endline "Error in check value, no type found"

  let rec process_new_types inputs : entry list =
    match inputs with
    | [] -> []
    | hd :: tl ->
        (match hd with
        | [] -> raise Stack_overflow
        | hd :: tl -> (
            let tle = match tl with a :: b -> a | _ -> raise Stack_overflow in
            match hd with
            | "string" -> Type (tle, Strings)
            | "int" -> Type (tle, Ints)
            | "bool" -> Type (tle, Bools)
            | "float" -> Type (tle, Floats)
            | "char" -> Type (tle, Chars)
            | _ -> raise Stack_overflow))
        :: process_new_types (List.tl inputs)

  let rec process_types types inputs =
    match types with
    | [] -> []
    | hd :: tl ->
        (match hd with
        | Type (name, typ) -> (
            match typ with
            | Strings -> String (List.hd inputs)
            | Floats -> Float (float_of_string (List.hd inputs))
            | Ints -> Int (int_of_string (List.hd inputs))
            | Chars -> Char (List.hd inputs).[0]
            | Bools -> Bool (bool_of_string (List.hd inputs))
            | Ids -> Id ("", String ""))
        | _ -> raise Stack_overflow)
        :: process_types tl (List.tl inputs)

  let process_list table entries database =
    match List.find (fun a -> match a with n, tt -> n = table) database with
    | _, [] -> raise Stack_overflow
    | _, types :: rest -> [ process_types types entries ]

  let rec add_entry table_name new_row database =
    match database with
    | [] -> raise Not_found
    | (a, b) :: tl ->
        if a = table_name then (a, Table.insert b new_row) :: tl
        else (a, b) :: add_entry table_name new_row tl
end
