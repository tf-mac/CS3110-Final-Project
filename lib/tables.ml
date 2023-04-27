open Utils

module type Table = sig
  type t

  exception IndexExists
  exception TypeMismatch

  val empty : entry list -> t
  val insert : t -> entry list -> t
  val at : t -> entry -> entry list
  val delete : t -> entry -> t
  val table_to_string : t -> string
  val header : t -> entry list
end

module ListTable : Table with type t = entry list list = struct
  type t = entry list list

  exception IndexExists
  exception TypeMismatch

  let empty (ex : entry list) = [ ex ]

  let insert (table : t) a =
    if List.filter (fun b -> match b with d -> d = a) table = [] then table
    else table

  let at (table : t) id =
    List.find
      (fun a ->
        match a with
        | [] -> raise (Failure "This shouldn't happen")
        | hd :: tl -> hd = id)
      table

  let delete (table : t) id =
    List.filter
      (fun a -> match a with [] -> false | a :: asd -> id <> a)
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

  let header = function
    | [] -> raise (Failure "RI Violated for tables")
    | hd :: tl -> hd
end
