open Database

let data = ref Database.empty

type input = Empty | Malformed | Valid of string list

let user_defined_types = ref []

let rec lst_to_string lst =
  match lst with [] -> "" | [ h ] -> h | h :: t -> h ^ " " ^ lst_to_string t

let check_value_defn x =
  match x with
  | "int" :: t -> Valid [ "int"; lst_to_string t |> String.trim ]
  | "char" :: t -> Valid [ "char"; lst_to_string t |> String.trim ]
  | "bool" :: t -> Valid [ "bool"; lst_to_string t |> String.trim ]
  | "float" :: t -> Valid [ "float"; lst_to_string t |> String.trim ]
  | "string" :: t -> Valid [ "string"; lst_to_string t |> String.trim ]
  | l ->
      if List.mem (List.hd l) !user_defined_types = true then
        Valid [ List.hd l; List.tl l |> lst_to_string ]
      else Malformed

let parse_value_defn x =
  match x with
  | "" -> Empty
  | s ->
      if String.contains s ' ' = false then Malformed
      else String.split_on_char ' ' s |> check_value_defn

let add_type x = user_defined_types := x :: !user_defined_types

let print_state string =
  print_string string;
  read_line ()

let rec read_value_defn line =
  match parse_value_defn line with
  | Empty -> []
  | Malformed -> print_state "   Invalid data types\n   " |> read_value_defn
  | Valid s ->
      print_string "   ";
      s :: read_value_defn (read_line ())

let parse_constructor_defn line =
  print_string "   ";
  line |> List.hd |> add_type;
  let table =
    Database.process_new_types
      ([ "string"; List.hd (List.tl line) ] :: (read_line () |> read_value_defn))
  in
  match data := Database.add_table !data table (List.hd line) with _ -> [ [] ]

let rec parse_value type_name line =
  if String.contains line '=' = false then
    print_state "   Must assign with '='\n   " |> parse_value type_name
  else
    let line_list = String.split_on_char '=' line in
    if line_list |> List.tl |> List.hd = "" then
      print_state "   Must have value after '='\n   " |> parse_value type_name
    else
      let entry_id = List.hd line_list |> String.trim in
      let entry_value = line_list |> List.tl |> lst_to_string |> String.trim in
      try
        Database.check_value !data type_name entry_id entry_value;
        entry_value
      with
      | Database.NoEntry ->
          print_state ("   No entry of name '" ^ entry_id ^ "'\n   ")
          |> parse_value type_name
      | Database.WrongType ->
          print_state ("   '" ^ entry_value ^ "' is not the correct type\n   ")
          |> parse_value type_name

let add_entry new_row table =
  match data := Database.add_entry table new_row !data with _ -> ()

let rec read_make type_name line =
  match line with
  | "" -> []
  | s ->
      let holyshitpleasegodhelp = parse_value type_name s in
      holyshitpleasegodhelp :: read_make type_name (print_state "   ")

let rec read_input line =
  if line = "quit" then ()
  else if List.hd (String.split_on_char ' ' line) = "save" then (
    match String.split_on_char ' ' line with
    | [] ->
        print_endline "This shouldn't be possible yet somhow it happened...";
        read_input (read_line ())
    | [ _ ] ->
        print_endline "File name required to store";
        read_input (read_line ())
    | _ :: file :: _ ->
        print_endline ("Saving to " ^ file ^ "...");
        let oc = Stdlib.open_out file in
        Stdlib.output_string oc (Database.db_to_string !data);
        Stdlib.flush oc;
        read_input (read_line ()))
  else if line = "help" then
    print_state
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
    |> read_input
  else if line = "print" then
    Database.db_to_string !data ^ "\n" |> print_state |> read_input
  else if String.contains line ' ' = false then
    print_state "Invalid Type, must include Type and ID\n" |> read_input
  else
    let input_list = String.split_on_char ' ' line in
    if List.mem (String.split_on_char ' ' line |> List.hd) !user_defined_types
    then (
      add_entry
        ((List.tl input_list |> lst_to_string)
        :: read_make (List.hd input_list) (print_state "   "))
        (List.hd input_list);
      read_line () |> read_input)
    else if input_list |> List.hd = "def" then
      if List.length input_list = 2 then
        print_state "Invalid Type definition, must include Type and ID\n"
        |> read_input
      else
        match input_list |> List.tl |> parse_constructor_defn with
        | _ -> read_line () |> read_input
    else print_state "Type does not exist\n" |> read_input

let rec process_commands input =
  (match List.hd (String.split_on_char ' ' input) with
  | "def" -> print_string ""
  | "print" -> print_string (Database.db_to_string !data)
  | _ -> print_endline "Unrecognized command, try help for a list of commands");
  process_commands (read_line ())

(** [main ()] prompts for the script to start, then starts it. *)

let main () =
  print_string
    "\n\n\
     Welcome to the 3110 Database Command Line\n\
     Please describe the data you want to store.\n\
     Type 'quit' to quit, 'help' for help.\n\n";
  read_line () |> read_input
