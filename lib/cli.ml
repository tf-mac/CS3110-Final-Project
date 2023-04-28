open Utils
open Database
open Tables
module Tbl = Tables.HashTable
module DB = Database (Tbl)

let db = ref DB.empty

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

let rec build_instance (name, table, vals) =
  print_string "|    ";
  let input = read_line () in
  match String.split_on_char '=' input with
  | [] | [ "" ] ->
      print_endline "|    <|\n";
      DB.add_named_entry name vals !db
  | "" :: tl ->
      print_endline "Please enter a non-empty name";
      build_instance (name, table, vals)
  | [ n ] ->
      print_endline "Please input a value";
      build_instance (name, table, vals)
  | n :: v :: tl -> (
      match List.assoc_opt n vals with
      | None -> (
          match Tbl.exists table n with
          | exception TypeMismatch ->
              print_endline "That field does not exist in this type";
              build_instance (name, table, vals)
          | t -> (
              match parse_value v t with
              | x -> build_instance (name, table, (n, x) :: vals)
              | exception ParseError ->
                  print_endline
                    "That value does not match the type of the field";
                  build_instance (name, table, vals)))
      | Some _ ->
          print_endline "This field has already been entered";
          build_instance (name, table, vals))

let process_assign = function
  | [] | [ "" ] -> print_endline "Please input a type name to assign to"
  | [ name ] -> print_endline "Please input an id for this instance"
  | name :: id :: tl -> (
      match DB.get_table name !db with
      | Some t ->
          build_instance
            ( name,
              t,
              [
                ( (match Tbl.header t with
                  | Type (n, _) :: tl -> n
                  | _ -> raise ParseError),
                  String id );
              ] )
      | None -> print_endline "That type does not exist")

let parse_type (typ, name) =
  match typ with
  | "int" -> Type (name, Ints)
  | "float" -> Type (name, Floats)
  | "string" -> Type (name, Strings)
  | "bool" -> Type (name, Bools)
  | "chars" -> Type (name, Chars)
  | "id" -> Type (name, Ids)
  | _ -> raise ParseError

let rec build_type (name, id, types) () =
  print_string "|    ";
  let input = read_line () in
  match String.split_on_char ' ' input with
  | [] | [ "" ] ->
      print_endline "|    <|\n";
      db := DB.build_table !db (Type (id, Strings) :: types) name
  | [ typ ] -> print_endline "Please enter a name for this field\n   "
  | typ :: field_name :: tl -> (
      match parse_type (typ, field_name) with
      | Type _ as t -> build_type (name, id, types @ [ t ]) ()
      | exception ParseError ->
          print_endline "Not a recognized type";
          build_type (name, id, types) ()
      | _ -> raise (Failure "Should be impossible"))

let process_type = function
  | [] | [ "" ] -> print_endline "Please enter a type name for the definition"
  | [ name ] -> print_endline "Please enter a name for the ID of this type"
  | name :: id :: tl -> (
      match DB.get_table name !db with
      | Some _ -> print_endline ("|    <|\n Type " ^ name ^ " already defined")
      | None -> build_type (name, id, []) ())

let rec parse_input input =
  (match String.split_on_char ' ' input with
  | "quit" :: tl -> exit 0
  | "help" :: tl ->
      print_endline
        "\n\
         To define a custom dataframe, type all valueNames must be unique:\n\
         def TypeName IdName\n\
        \   type valueName\n\
        \   ...\n\
        \   type valueName\n\n\n\
         To assign values to the custom types, all values must be assigned,\n\
         in order of their definition:\n\
         TypeName IdValue\n\
        \   valueName = value\n\
        \   ...\n\
        \   valueName = value\n\n\
         To save to a file, use 'save <file>'\n"
  | "def" :: tl -> process_type tl
  | "assign" :: tl -> process_assign tl
  | "print" :: tl -> print_endline (DB.db_to_string !db)
  | _ -> exit 0);
  print_string "|> ";
  read_line () |> parse_input

(** [main ()] prompts for the script to start, then starts it. *)

let main () =
  print_string
    "\n\n\
     Welcome to the 3110 Database Command Line\n\
     Please describe the data you want to store.\n\
     Type 'quit' to quit, 'help' for help.\n\n";
  print_string "|> ";
  read_line () |> parse_input
