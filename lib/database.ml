module type Table = sig
  type t
  type value

  val empty : value -> t
  val insert : t -> value -> t
  val at : t -> int -> value
  val delete : t -> int -> t
  val table_to_string : t -> string
end

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * string)

let entry_to_string ent =
  match ent with
  | String x -> x
  | Float x -> string_of_float x
  | Int x -> string_of_int x
  | Char x -> String.make 1 x
  | Bool x -> string_of_bool x
  | Id (a, b) -> a ^ "@" ^ b

module ListOfTupleTable :
  Table with type t = (int * entry list) list and type value = entry list =
struct
  type t = (int * entry list) list
  type value = entry list

  let rec index (id : int) table curr =
    match table with
    | [] -> raise (Arg.Bad (string_of_int id))
    | x :: ta ->
        if match x with a, b -> id = a then match x with a, b -> b
        else index id ta (curr + 1)

  let rec size table cnt =
    match table with [] -> cnt | x :: xs -> size xs (cnt + 1)

  let empty (ex : entry list) = [ (0, ex) ]

  let insert (table : t) a =
    if List.filter (fun b -> match b with c, d -> d = a) table = [] then
      (size table 0, a) :: table
    else table

  let at (table : t) id = index id table 0

  let delete (table : t) id =
    List.filter (fun a -> match a with ida, data -> id <> ida) table

  let rec table_to_string (table : t) =
    let rec build_row entlist =
      match entlist with
      | [] -> "\n"
      | x :: xs -> entry_to_string x ^ "\t" ^ build_row xs
    in
    match table with
    | [] -> ""
    | (a, b) :: xs -> build_row b ^ table_to_string xs ^ "\n"
end

module Database = struct
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
          id
    | _ -> raise (Arg.Bad "Reference in bad format")

  let rec db_to_string database =
    match database with
    | [] -> "\n"
    | (name, x) :: xs ->
        "Table:\t" ^ name ^ "\n" ^ T.table_to_string x ^ db_to_string xs
end
