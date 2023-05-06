module type CliHandler = sig
  val parse_input : string -> string
end

module CLI = struct
  open Utils
  open Database
  open Tables
  module Tbl = Tables.HashTable
  module DB = Database (Tbl)

  let db = ref DB.empty

  type state =
    | Default
    | BuildInstance of (string * Tbl.t * (string * entry) list)
    | BuildType of (string * string * entry list)

  let current_state = ref Default

  exception ParseError

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
    | Ids -> raise ParseError

  let rec build_instance (name, table, vals) input =
    match input |> String.split_on_char '=' |> List.map String.trim with
    | [] | [ "" ] ->
        DB.add_named_entry name vals !db;
        current_state := Default;
        "|    <|\n"
    | "" :: tl ->
        current_state := BuildInstance (name, table, vals);
        "Please enter a non-empty name\n|    "
    | [ n ] ->
        current_state := BuildInstance (name, table, vals);
        "Please input a value\n|    "
    | n :: v :: tl -> (
        match List.assoc_opt n vals with
        | None -> (
            match Tbl.exists table n with
            | exception TypeMismatch ->
                current_state := BuildInstance (name, table, vals);
                "That field does not exist in this type\n|    "
            | t -> (
                match parse_value v t with
                | x ->
                    current_state := BuildInstance (name, table, (n, x) :: vals);
                    "|    "
                | exception ParseError ->
                    current_state := BuildInstance (name, table, vals);
                    "That value does not match the type of the field\n|    "))
        | Some _ ->
            current_state := BuildInstance (name, table, vals);
            "This field has already been entered\n|    ")

  let process_assign = function
    | [] | [ "" ] -> "Please input a type name and id"
    | [ name ] -> "Please input an id for this instance"
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
                      | _ -> raise ParseError),
                      String id );
                  ] );
            "|    "
        | None -> "That type does not exist")

  let parse_type (typ, name) =
    match typ with
    | "int" -> Type (name, Ints)
    | "float" -> Type (name, Floats)
    | "string" -> Type (name, Strings)
    | "bool" -> Type (name, Bools)
    | "chars" -> Type (name, Chars)
    | "id" -> Type (name, Ids)
    | _ -> raise ParseError

  let rec build_type (name, id, types) input =
    match String.split_on_char ' ' input with
    | [] | [ "" ] ->
        db := DB.build_table !db (Type (id, Strings) :: types) name;
        current_state := Default;
        "|    <|\n"
    | [ typ ] -> "Please enter a name for this field\n|    "
    | typ :: field_name :: tl -> (
        match parse_type (typ, field_name) with
        | Type _ as t ->
            current_state := BuildType (name, id, types @ [ t ]);
            "|    "
        | exception ParseError ->
            current_state := BuildType (name, id, types);
            "Not a recognized type\n|    "
        | _ -> raise (Failure "Should be impossible"))

  let process_type = function
    | [] | [ "" ] -> "Please enter a type name for the definition\n|> "
    | [ name ] -> "Please enter a name for the ID of this type\n|> "
    | name :: id :: tl -> (
        match DB.get_table name !db with
        | Some _ -> "|    <|\n Type " ^ name ^ " already defined\n|    "
        | None ->
            current_state := BuildType (name, id, []);
            "|    ")

  let process_at = function
    | [] | [ "" ] ->
        "Please enter what the type and id of which to get an instance\n|> "
    | [ name ] -> "Please enter an id of the instance you which to get\n|> "
    | name :: id :: tl -> (
        match DB.get_table name !db with
        | Some x ->
            (x |> Tbl.header |> optionize |> build_row)
            ^ "\n"
            ^ (String id |> Tbl.at x |> build_row)
        | None -> "No type named " ^ name)

  (** [parse_input input] takes in new input and determines the relevant command*)
  let parse_input input =
    match !current_state with
    | BuildInstance v -> build_instance v input
    | BuildType v -> build_type v input
    | Default -> (
        match String.split_on_char ' ' input with
        | "quit" :: tl -> exit 0
        | "help" :: tl ->
            "\n\
             To define a custom dataframe, type all valueNames must be unique:\n\
             def TypeName IdName\n\
            \   type valueName\n\
            \   ...\n\
            \   type valueName\n\n\n\
             To assign values to the custom types:\n\
             assign TypeName IdValue\n\
            \   valueName = value\n\
            \   ...\n\
            \   valueName = value\n\n\
             To save to a file, use 'save <file>'\n\n\
             |> "
        | "def" :: tl -> process_type tl
        | "assign" :: tl -> process_assign tl
        | "print" :: tl -> DB.db_to_string !db ^ "\n|> "
        | "at" :: tl -> process_at tl ^ "\n|> "
        | _ -> "Unknown command. Type help for a list of commands\n|> ")
end

(** [main ()] prompts for the script to start, then starts it. *)

let main () =
  print_string
    "\n\n\
     Welcome to the 3110 Database Command Line\n\
     Please describe the data you want to store.\n\
     Type 'quit' to quit, 'help' for help. new \n\n";
  print_string "|> ";
  while true do
    read_line () |> CLI.parse_input |> print_string
  done
