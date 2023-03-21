open Database

let data = ref Database.empty

type input = Empty | Malformed | Valid of string list

let user_defined_types = ref []

let rec lst_to_string lst =
  match lst with [] -> "" | [ h ] -> h | h :: t -> h ^ " " ^ lst_to_string t

let check_value_defn x =
  match x with
  | "int" :: t -> Valid [ "int"; lst_to_string t ]
  | "char" :: t -> Valid [ "char"; lst_to_string t ]
  | "bool" :: t -> Valid [ "bool"; lst_to_string t ]
  | "float" :: t -> Valid [ "float"; lst_to_string t ]
  | "string" :: t -> Valid [ "string"; lst_to_string t ]
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
  let table = Database.process_new_types (read_line () |> read_value_defn) in
  match data := Database.add_table !data table (List.hd line) with _ -> [ [] ]

let rec parse_value type_name line : string =
  let line_list = String.split_on_char '=' line in
  let entry_id = List.hd line_list in
  let entry_value = line_list |> List.tl |> lst_to_string in
  match Database.check_value !data type_name entry_id entry_value with
  | exception Database.NoEntry ->
      print_state "   No entry of name '" ^ entry_id ^ "'\n"
      |> parse_value type_name
  | exception Database.WrongType ->
      print_state "   '" ^ entry_value ^ "' is not the correct type\n"
      |> parse_value type_name
  | _ -> ( match line_list with [ a; b ] -> b | _ -> raise Stack_overflow)

let add_entry new_row table =
  print_string ("Adding to " ^ table);
  match data := Database.add_entry table new_row !data with
  | _ -> print_string (Database.db_to_string !data)

let rec read_make type_name line =
  match line with
  | "" -> []
  | s -> parse_value type_name line :: read_make type_name (print_state "   ")

let rec make_entires type_name inputs = 
  match inputs with
  | [] -> []
  | a :: b -> parse_value type_name a :: make_entires type_name b

let rec read_input line =
  if line = "print" then print_string (Database.db_to_string !data)
  else if List.hd (String.split_on_char ' ' line) = "save" then match String.split_on_char ' ' line with 
  | [] -> print_endline "This shouldn't be possible yet somhow it happened..."; read_input (read_line () )
  | _ :: [] -> print_endline "File name required to store"; read_input (read_line () )
  | _ :: file :: _ -> print_endline ("Saving to " ^ file ^ "..."); let oc = Stdlib.open_out file in Stdlib.output_string oc (Database.db_to_string !data); Stdlib.flush oc; read_input (read_line ())
  else if String.contains line ' ' = false then
    print_state "Invalid Type, must include Type and ID\n" |> read_input
  else
    let input_list = String.split_on_char ' ' line in
    if List.mem (String.split_on_char ' ' line |> List.hd) !user_defined_types
    then (
      add_entry (make_entires (List.hd (input_list)) (List.tl input_list)) (List.hd (input_list));
      read_line () |> read_input)
    else if input_list |> List.hd = "def" then
      if List.length input_list = 2 then
        print_state "Invalid Type definition, must include Type and ID\n" |> read_input
      else
        match input_list |> List.tl |> parse_constructor_defn with
        | _ -> read_input (read_line ())
    else print_state "Type does not exist\n" |> read_input

(** [main ()] prompts for the script to start, then starts it. *)
let main () =
  print_string "\n\nWelcome to the 3110 Database Command Line\n";
  print_endline
    "Please describe the data you want to store. To define a custom dataframe, \
     type:\n\
     def TypeName id\n\
    \   type valueName\n\
    \   ...\n\
    \   type valueName\n\n\n";
  read_line () |> read_input

(* Execute the CLI. *)
let () = main ()
