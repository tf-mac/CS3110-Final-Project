module type Table = sig
  type t
  type value

  val empty : value -> t
  val insert : t -> value -> t
  val at : t -> int -> value
  val delete : t -> int -> t
end

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Id of (string * int)

module ListOfTupleTable :
  Table with type t = (int * entry) list and type value = entry = struct
  type t = (int * entry) list
  type value = entry

  let rec index (id : int) table curr =
    match table with
    | [] -> raise (Arg.Bad (string_of_int id))
    | x :: ta ->
        if match x with a, b -> id = a then match x with a, b -> b
        else index id ta (curr + 1)

  let rec size table cnt =
    match table with [] -> cnt | x :: xs -> size xs (cnt + 1)

  let empty (ex : value) = [ (0, ex) ]

  let insert (table : t) a =
    if List.filter (fun b -> match b with c, d -> d = a) table = [] then
      (size table 0, a) :: table
    else table

  let at (table : t) id = index id table 0

  let delete (table : t) id =
    List.filter (fun a -> match a with ida, data -> id <> ida) table
end

module Database (T : Table) = struct
  let empty = []

  let add_table database ex name =
    match List.find (fun a -> match a with tname, t -> name = tname) with
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
end
