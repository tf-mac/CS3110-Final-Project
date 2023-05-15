module type CliHandler = sig
  val parse_input : string -> string
  val get_response : string -> string
  val reset : unit -> unit
end

module CLI = struct
  open Utils
  open Database
  open Tables
  module Tbl = Tables.HashTable
  module DB = Database (Tbl)
  open Yojson.Basic.Util

  let db = ref DB.empty

  type state =
    | Default
    | BuildInstance of (string * Tbl.t * (string * entry) list)
    | BuildType of (string * string * entry list)

  exception ParseError
  exception InvalidExpr
  exception InvalidComparison

  let current_state = ref Default
  let file_name = "data/responses.json"

  let get_response response =
    let file = Yojson.Basic.from_file file_name in
    file |> to_assoc |> List.assoc response |> to_string

  let reset () =
    current_state := Default;
    db := DB.empty

  let parse_value v = function
    | Strings -> String v
    | Ints -> (
        match int_of_string_opt v with
        | Some i -> Int i
        | None -> raise ParseError)
    | Floats -> (
        match float_of_string_opt v with
        | Some f -> Float f
        | None -> raise ParseError)
    | Bools -> (
        match bool_of_string_opt v with
        | Some b -> Bool b
        | None -> raise ParseError)
    | Chars -> Char (if String.length v = 1 then v.[0] else raise ParseError)
    | Ids ->
        Id
          (match v |> String.split_on_char '@' |> List.map String.trim with
          | [] | [ _ ] | _ :: _ :: _ :: _ -> raise ParseError
          | [ hd; tl ] -> (hd, String tl))

  let rec build_instance (name, table, vals) input =
    match
      input |> String.split_on_char '=' |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    with
    | [] ->
        DB.add_named_entry name vals !db;
        current_state := Default;
        get_response "indent_end" (* "|    <|\n|> " *)
    | [ n ] ->
        current_state := BuildInstance (name, table, vals);
        get_response "err_create_field_no_value"
        (* "Please input a variable and a value\n|    " *)
    | n :: v :: tl -> (
        match List.assoc_opt n vals with
        | None -> (
            match Tbl.exists table n with
            | exception TypeMismatch ->
                current_state := BuildInstance (name, table, vals);
                get_response "err_create_field_DNE"
                (* "That field does not exist in this type\n|    " *)
            | t -> (
                match parse_value v t with
                | x ->
                    current_state := BuildInstance (name, table, (n, x) :: vals);
                    get_response "indent" (* "|    " *)
                | exception ParseError ->
                    current_state := BuildInstance (name, table, vals);
                    get_response "err_create_field_wrong_type"
                    (* "That value does not match the type of the field\n|    " *)
                ))
        | Some _ ->
            current_state := BuildInstance (name, table, vals);
            get_response "err_create_field_already_entered"
            (* "This field has already been entered\n|    " *))

  let process_assign input =
    match input |> List.map String.trim |> List.filter (fun s -> s <> "") with
    | [] ->
        get_response "err_assign_empty"
        (* "Please input a type name and id\n|> " *)
    | [ name ] ->
        get_response "err_assign_no_id"
        (* "Please input an id for this instance\n|> " *)
    | name :: id :: tl -> (
        match DB.get_table name !db with
        | Some t ->
            current_state :=
              BuildInstance
                ( name,
                  t,
                  [
                    ( (match Tbl.header t with
                      | Type (n, _) :: tl -> n
                      | _ -> failwith "impossible" [@coverage off]),
                      String id );
                  ] );
            get_response "indent"
        | None -> get_response "err_assign_DNE")
  (* "That type does not exist\n|> " *)

  let parse_type (typ, name) =
    match typ with
    | "int" -> Type (name, Ints)
    | "float" -> Type (name, Floats)
    | "string" -> Type (name, Strings)
    | "bool" -> Type (name, Bools)
    | "char" -> Type (name, Chars)
    | "id" -> Type (name, Ids)
    | _ -> raise ParseError

  let rec build_type (name, id, types) input =
    match
      String.split_on_char ' ' input
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    with
    | [] ->
        db := DB.build_table !db (Type (id, Strings) :: types) name;
        current_state := Default;
        get_response "indent_end" (* "|    <|\n|> " *)
    | [ typ ] -> get_response "err_defn_no_name"
    (* "Please enter a name for this field\n|    " *)
    | typ :: field_name :: tl -> (
        match parse_type (typ, field_name) with
        | Type _ as t ->
            current_state := BuildType (name, id, types @ [ t ]);
            get_response "indent" (* "|    " *)
        | exception ParseError ->
            current_state := BuildType (name, id, types);
            get_response
              "err_defn_invalid_type" (* "Not a recognized type\n|    " *)
        | _ -> raise (Failure "Should be impossible") [@coverage off])

  let process_type input =
    match List.filter (fun s -> s <> "") input with
    | [] -> get_response "err_defn_needs_type_name"
    (* "Please enter a type name for the definition\n|> " *)
    | [ name ] ->
        get_response "err_defn_needs_ID_name"
        (* "Please enter a name for the ID of this type\n|> " *)
    | name :: id :: tl -> (
        match DB.get_table name !db with
        | Some _ ->
            get_response "err_defn_already_exists"
            (* "\n Type already defined\n|> " *)
        | None ->
            current_state := BuildType (name, id, []);
            get_response "indent" (* "|    " *))

  let process_at input =
    match input |> List.map String.trim |> List.filter (fun s -> s <> "") with
    | [] ->
        get_response "err_at_empty"
        (* "Please enter what the type and id of which to get an instance\n|> " *)
    | [ name ] ->
        get_response "err_at_no_id"
        (* "Please enter an id of the instance you which to get\n|> " *)
    | [ name; id ] -> (
        match DB.get_table name !db with
        | Some x -> (
            try
              (x |> Tbl.header |> optionize |> build_row)
              ^ "\n"
              ^ (String id |> Tbl.at x |> build_row)
              ^ "\n|> "
            with Not_found -> get_response "err_at_id_DNE")
        | None ->
            get_response "err_at_invalid_type" (* "No type of that name" *))
    | name :: id :: col :: tl -> (
        match DB.get_table name !db with
        | Some x -> (
            try
              let row = Tbl.at x (String id) in
              match int_of_string_opt col with
              | None ->
                  get_response "err_at_column_not_int"
                  (* "Column number should be an int" *)
              | Some i -> (
                  match List.nth_opt row i with
                  | Some e -> (
                      match e with
                      | None -> get_response "no_entry"
                      | Some e -> (
                          match e with
                          | Id (name, row) ->
                              (entry_to_string e ^ "="
                              ^
                              match DB.get_reference e !db with
                              | exception Not_found ->
                                  get_response "unbound_type"
                              | l, r -> (
                                  "\n"
                                  ^ build_row (optionize l)
                                  ^
                                  match r with
                                  | None -> get_response "unbound_val"
                                  | Some v -> build_row v))
                              ^ "\n|> "
                          | _ -> entry_to_string e ^ "\n|> "))
                  | None -> get_response "err_at_column_out_of_range")
            with Not_found -> get_response "err_at_id_DNE")
        | None -> get_response "err_at_invalid_type")

  let split_on_substring sub str =
    let idxs = ref [ 0 ] in
    let sub_len = String.length sub in
    for i = 0 to String.length str - sub_len do
      if String.sub str i sub_len = sub then
        idxs := !idxs @ [ i; i + String.length sub ]
      else ()
    done;
    idxs := !idxs @ [ String.length str ];
    let rec create_lst idxs sub_len str =
      match idxs with
      | [] -> []
      | s :: e :: t -> String.sub str s (e - s) :: create_lst t sub_len str
      | _ -> failwith "odd" [@coverage off]
    in
    create_lst !idxs sub_len str

  let parse_compare_exp str =
    let str_lst = String.split_on_char ' ' str in
    if List.length str_lst <> 3 then raise InvalidExpr
    else
      match str_lst with
      | [ var; compare; value ] ->
          ( var,
            (match compare with
            | "=" -> EQ
            | "<>" -> NEQ
            | ">" -> GT
            | "<" -> LT
            | "<=" -> LTE
            | ">=" -> GTE
            | _ -> raise InvalidComparison),
            value )
      | _ -> failwith "should be impossible" [@coverage off]

  let process_find lst =
    let cleaned_lst =
      lst |> List.map String.trim |> List.filter (fun s -> s <> "")
    in
    if cleaned_lst = [] then get_response "err_find_missing_type"
    else
      match DB.get_table (List.hd cleaned_lst) !db with
      | None -> get_response "err_find_invalid_type"
      | Some type_table -> (
          try
            cleaned_lst |> List.tl
            |> List.fold_left (fun s1 s2 -> s1 ^ " " ^ s2) ""
            |> split_on_substring "and" |> List.map String.trim
            |> List.filter (fun s -> s <> "")
            |> (fun lst -> if lst = [] then raise InvalidExpr else lst)
            |> List.map parse_compare_exp
            |> Tbl.process_constraints type_table
          with
          | InvalidExpr -> get_response "err_find_invalid_expr"
          | InvalidComparison -> get_response "err_find_invalid_comparison"
          | TypeMismatch -> get_response "err_find_wrong_type"
          | Not_found -> get_response "err_find_var_DNE")

  (** [parse_input input] takes in new input and determines the relevant command*)
  let parse_input input =
    match !current_state with
    | BuildInstance v -> build_instance v input
    | BuildType v -> build_type v input
    | Default -> (
        match String.split_on_char ' ' input with
        | "quit" :: tl -> exit 0 [@coverage off]
        | "help" :: tl -> get_response "help_message"
        | "def" :: tl -> process_type tl
        | "assign" :: tl -> process_assign tl
        | "print" :: tl -> DB.db_to_string !db ^ "|> "
        | "at" :: tl -> process_at tl
        | "find" :: tl -> process_find tl
        | _ ->
            get_response "err_unknown_command"
            (* "Unknown command. Type help for a list of commands\n|> " *))
end

(** [main ()] prompts for the script to start, then starts it. *)

let main () =
  let file_name = "data/responses.json" in
  let welcom_string =
    let file = Yojson.Basic.from_file file_name in
    file |> Yojson.Basic.Util.to_assoc |> List.assoc "welcome"
    |> Yojson.Basic.Util.to_string
  in
  print_string welcom_string;
  print_string "|> ";
  while true do
    read_line () |> CLI.parse_input |> print_string
  done
  [@@coverage off]
