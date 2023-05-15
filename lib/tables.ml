open Utils

let rec assert_types header a =
  match header with
  | [] -> if a = [] then [] else raise TypeMismatch [@coverage off]
  | hd :: tl -> (
      match a with
      | [] -> raise TypeMismatch
      | b :: c ->
          (match hd with
          | Some (Type (_, Strings)) -> (
              match b with
              | Some (String _) -> b
              | None -> b
              | _ -> raise TypeMismatch)
          | Some (Type (_, Floats)) -> (
              match b with
              | Some (Float _) -> b
              | None -> b
              | _ -> raise TypeMismatch)
          | Some (Type (_, Ints)) -> (
              match b with
              | Some (Int _) -> b
              | None -> b
              | _ -> raise TypeMismatch)
          | Some (Type (_, Bools)) -> (
              match b with
              | Some (Bool _) -> b
              | None -> b
              | _ -> raise TypeMismatch)
          | Some (Type (_, Chars)) -> (
              match b with
              | Some (Char _) -> b
              | None -> b
              | _ -> raise TypeMismatch)
          | Some (Type (_, Ids)) -> (
              match b with
              | Some (Id _) -> b
              | None -> b
              | _ -> raise TypeMismatch)
          | _ -> raise TypeMismatch)
          :: assert_types tl c)

let rec reorder_list (a : (string * entry) list) = function
  | [] -> if a = [] then [] else raise TypeMismatch
  | Some (Type (name, v)) :: tl ->
      let vl, rest =
        let rec extr_name acc = function
          | [] -> (None, acc)
          | (n, vr) :: rest ->
              if n = name then (Some vr, acc @ rest)
              else extr_name (acc @ [ (n, vr) ]) rest
        in
        extr_name [] a
      in
      vl :: reorder_list rest tl
  | _ -> raise TypeMismatch

let rec get_type_index cnt name = function
  | [] -> raise Not_found
  | Type (n, _) :: tl ->
      if n = name then cnt else get_type_index (cnt + 1) name tl
  | _ -> raise TypeMismatch

module type Table = sig
  type t

  val empty : entry list -> t
  val insert : t -> entry list -> t
  val insert_named : t -> (string * entry) list -> t
  val at : t -> entry -> entry option list
  val delete : t -> entry -> t
  val table_to_string : t -> string
  val process_constraints : t -> (string * comparison * string) list -> string
  val header : t -> entry list
  val exists : t -> string -> types
end

module ListTable : Table = struct
  type t = entry option list list

  let rec optionize = function [] -> [] | hd :: tl -> Some hd :: optionize tl
  let empty (ex : entry list) = [ optionize ex ]

  let insert (table : t) a =
    match
      List.find
        (fun b ->
          match optionize a with
          | [] -> false
          | hd :: tl -> ( match b with [] -> false | hdb :: tlb -> hd = hdb))
        table
    with
    | exception Not_found ->
        table @ [ assert_types (List.hd table) (optionize a) ]
    | x -> raise IndexExists

  let insert_named table elist =
    let name, value = List.hd elist in
    match List.find (fun a -> List.hd a = Some value) table with
    | x -> raise IndexExists
    | exception Not_found ->
        table
        @ [ assert_types (List.hd table) (reorder_list elist (List.hd table)) ]

  let at (table : t) id =
    List.find
      (fun a ->
        match a with
        | [] -> raise Not_found
        | hd :: tl -> ( match hd with Some x -> x = id | None -> false))
      table

  let delete (table : t) id : t =
    List.filter
      (fun a ->
        match a with
        | [] -> true
        | a :: asd -> ( match a with Some x -> x <> id | None -> true))
      table

  let rec table_to_string (table : t) =
    match table with
    | [] -> ""
    | b :: xs -> build_row b ^ table_to_string xs ^ "\n"

  let rec deoptionize = function
    | [] -> []
    | hd :: tl ->
        (match hd with
        | Some x -> x
        | None -> raise (Failure "Deoptionize saw None") [@coverage off])
        :: deoptionize tl

  let header = function
    | [] -> raise (Failure "RI Violated for tables") [@coverage off]
    | hd :: tl -> deoptionize hd

  let exists table name =
    let rec follow_header = function
      | [] -> raise TypeMismatch
      | Type (n, t) :: tl when n = name -> t
      | _ :: tl -> follow_header tl
    in
    follow_header (header table)

  let rec process_constraints tbl lst =
    match lst with
    | [] -> table_to_string tbl
    | hd :: tl ->
        let ntbl =
          match hd with
          | name, cmp, vl -> (
              let ind = get_type_index 0 name (header tbl) in
              match List.nth (header tbl) ind with
              | Type (_, t) ->
                  let e = process_entry vl t in
                  List.filter
                    (fun a ->
                      match List.nth a ind with
                      | None -> false
                      | Some v -> run_constraint cmp e v)
                    tbl
              | _ -> failwith "Impossible")
        in
        process_constraints ntbl tl
end

module HashTable = struct
  type t = HashTab of entry list * (entry, entry option list) Hashtbl.t

  let rec deoptionize_list = function
    | [] -> []
    | Some x :: tl -> x :: deoptionize_list tl
    | None :: tl -> raise (Failure "Deoptionize on none")
    [@@coverage off]

  let header = function HashTab (hd, _) -> hd
  let hshtable = function HashTab (_, hsh) -> hsh

  let deoptionize = function
    | Some x -> x
    | None -> raise (Failure "Deoptionize on none") [@coverage off]

  let rec optionize = function [] -> [] | hd :: tl -> Some hd :: optionize tl
  let empty (ex : entry list) = HashTab (ex, Hashtbl.create 0)

  let insert table entries =
    match Hashtbl.find_opt (hshtable table) (List.hd entries) with
    | Some x -> raise IndexExists
    | None ->
        let copy = Hashtbl.copy (hshtable table) in
        let entrs =
          assert_types (optionize (header table)) (optionize entries)
        in
        HashTab
          ( header table,
            (Hashtbl.add copy (List.hd entries) (List.tl entrs);
             copy) )

  let insert_named table entries =
    let name, value = List.hd entries in
    match Hashtbl.find_opt (hshtable table) value with
    | Some x -> raise IndexExists
    | None ->
        let copy = Hashtbl.copy (hshtable table) in
        let reordered =
          assert_types
            (header table |> optionize)
            (reorder_list entries (optionize (header table)))
        in
        Hashtbl.add copy (deoptionize (List.hd reordered)) (List.tl reordered);
        HashTab (header table, copy)

  let at table id = Some id :: Hashtbl.find (hshtable table) id

  let delete table id =
    let out = Hashtbl.copy (hshtable table) in
    Hashtbl.remove (hshtable table) id;
    HashTab (header table, out)

  let rec table_to_string table =
    Hashtbl.fold
      (fun id ent acc -> acc ^ shorten (entry_to_string id) ^ build_row ent)
      (hshtable table)
      (build_row (optionize (header table)))

  let rec process_constraints tbl lst =
    let newHash = Hashtbl.create 0 in
    match lst with
    | [] -> table_to_string tbl
    | hd :: tl ->
        process_constraints
          (match hd with
          | name, cmp, vl ->
              let ind = get_type_index 0 name (header tbl) in
              let cmp_func =
                match
                  List.find
                    (function
                      | Type (n, t) -> n = name | _ -> failwith "Impossible")
                    (header tbl)
                with
                | Type (n, t) -> (
                    match process_entry vl t with e -> run_constraint cmp e)
                | _ -> raise (Failure "Impossible")
              in
              Hashtbl.iter
                (fun a b ->
                  if ind = 0 then
                    if cmp_func a then Hashtbl.add newHash a b else ()
                  else
                    match List.nth b (ind - 1) with
                    | None -> ()
                    | Some v ->
                        if cmp_func v then Hashtbl.add newHash a b else ())
                (hshtable tbl);
              HashTab (header tbl, newHash))
          tl

  let exists table name =
    let rec follow_header = function
      | [] -> raise TypeMismatch
      | Type (n, t) :: tl when n = name -> t
      | _ :: tl -> follow_header tl
    in
    follow_header (header table)
end
