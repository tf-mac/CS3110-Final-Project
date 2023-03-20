open Database

let data = Database.empty

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
  line :: (read_line () |> read_value_defn)

let rec parse_value type_name line =
  let line_list = String.split_on_char '=' line in
  let entry_id = List.hd line_list in
  let entry_value = line_list |> List.tl |> lst_to_string in
  try Database.check_value data type_name entry_id entry_value with
  | Database.NoEntry ->
      print_state "No entry of name '" ^ entry_id ^ "'\n"
      |> parse_value type_name
  | Database.WrongType ->
      print_state "'" ^ entry_value ^ "' is not the correct type\n"
      |> parse_value type_name
  | _ -> line_list

let read_make  = 

let rec read_input line =
  if String.contains line ' ' = false then
    print_state "Invalid Type, must include ID" |> read_input
  else 
    let input_list = String.split_on_char ' ' line in
    if
    List.mem (String.split_on_char ' ' line |> List.hd) !user_defined_types
    = true
  then read_line () |> read_make (List.hd input_list)
  else
    if input_list |> List.hd = "def" then
      if List.length input_list = 2 then
        print_state "Invalid Type definition, must include ID" |> read_input
      else input_list |> List.tl |> parse_constructor_defn
    else print_state "Type does not exist" |> read_input

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
  read_line () |> read_input |> ignore

(* Execute the CLI. *)
let () = main ()
