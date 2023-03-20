module type Table = sig
  type t
  type value

  val empty : value -> t
  val insert : t -> value -> t
  val at : t -> value -> value
  val delete : t -> value -> t
  val table_to_string : t -> string
end

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * entry)

let rec entry_to_string ent =
  match ent with
  | String x -> x
  | Float x -> string_of_float x
  | Int x -> string_of_int x
  | Char x -> String.make 1 x
  | Bool x -> string_of_bool x
  | Id (a, b) -> a ^ "@" ^ entry_to_string b
  | Type (a, b) -> a

module ListOfTupleTable :
  Table with type t = entry list list and type value = entry list = struct
  type t = entry list list
  type value = entry list

  let rec index (id : entry list) table curr =
    match table with
    | [] -> raise (Arg.Bad "Index failed")
    | x :: ta ->
        if match x with b -> id = b then match x with b -> b
        else index id ta (curr + 1)

  let rec size table cnt =
    match table with [] -> cnt | x :: xs -> size xs (cnt + 1)

  let empty (ex : entry list) = [ ex ]

  let insert (table : t) a =
    if List.filter (fun b -> match b with d -> d = a) table = [] then table
    else table

  let at (table : t) id = index id table 0

  let delete (table : t) id =
    List.filter
      (fun a ->
        match a with
        | [] -> false
        | a :: asd -> ( match id with ids :: ida -> ids <> a | _ -> false))
      table

  let rec table_to_string (table : t) =
    let rec build_row entlist =
      match entlist with
      | [] -> "\n"
      | x :: xs -> entry_to_string x ^ "\t" ^ build_row xs
    in
    match table with
    | [] -> ""
    | b :: xs -> build_row b ^ table_to_string xs ^ "\n"
end

module Database = struct
  exception NoEntry
  exception WrongType

  module T = ListOfTupleTable

  let empty = []

  let add_table database (ex : entry list) name =
    match
      List.find (fun a -> match a with tname, t -> name = tname) database
    with
    | exception Not_found -> (name, T.empty ex) :: database
    | _ -> raise (Arg.Bad "Name already in database")

  let drop_table name database =
    List.filter (fun a -> match a with tname, t -> name <> tname) database

  let get_table name database =
    List.find (fun a -> match a with tname, t -> name = tname) database

  (*Currently doesn't work...*)
  let get_reference ent database =
    match ent with
    | Id (name, id) ->
        T.at
          (match
             List.find
               (fun a -> match a with tname, _ -> name = tname)
               database
           with
          | _, t -> t)
          [ id ]
    | _ -> raise (Arg.Bad "Reference in bad format")

  let rec db_to_string database =
    match database with
    | [] -> "\n"
    | (name, x) :: xs ->
        "Table:\t" ^ name ^ "\n" ^ T.table_to_string x ^ db_to_string xs

  let check_value (database : (string * T.t) list) tn eid ev =
    match List.find (fun a -> tn = a) database with
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
                | Int _ -> (
                    match int_of_string_opt ev with
                    | None -> raise WrongType
                    | Some _ -> ())
                | Float _ -> (
                    match float_of_string_opt ev with
                    | None -> raise WrongType
                    | Some _ -> ())
                | String _ -> ()
                | Char _ -> ()
                | Bool _ -> (
                    match bool_of_string_opt ev with
                    | None -> raise WrongType
                    | Some _ -> ())
                | _ -> ())
            | _ -> raise Stack_overflow))
end
