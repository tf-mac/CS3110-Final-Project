type input = 
| Empty
| Malformed
| Valid of string list
let user_defined_types = ref []

let rec lst_to_string lst =
  match lst with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ " " ^ lst_to_string t

let check_value_defn x = match x with
| "int" :: t -> Valid ["int"; lst_to_string t]
| "char" :: t -> Valid ["char"; lst_to_string t]
| "bool" :: t -> Valid ["bool"; lst_to_string t]
| "float" :: t -> Valid ["float"; lst_to_string t]
| "string" :: t -> Valid ["string"; lst_to_string t]
| l -> if List.mem (List.hd l) !user_defined_types = true 
  then Valid [List.hd l ; List.tl l |> lst_to_string] 
  else Malformed

let parse_value_defn x = match x with
| "" -> Empty
| s -> if String.contains s ' ' = false 
  then Malformed 
  else String.split_on_char ' ' s |> check_value_defn

let add_type x = user_defined_types := x :: !user_defined_types

let print_state string v =
  print_string (string ^ "\n\n"); v
          
let rec read_value_defn line = match parse_value_defn line with
  | Empty -> []
  | Malformed -> read_line () |> read_value_defn |> print_state "Invalid data types"
  | Valid s -> s ::  read_value_defn (read_line ())

  let parse_constructor_defn line = line |> List.hd |> add_type;
    line :: (read_line () |> read_value_defn)
  

let read_make line = [[]]

let rec read_input line =
  if String.contains line ' ' = false 
    then read_line () |> print_state "Invalid Type, must include ID" |> read_input 
else if List.mem (String.split_on_char ' ' line |> List.hd) !user_defined_types = true
  then read_line () |> read_make
else let input_list = String.split_on_char ' ' line in
if input_list |> List.hd = "def"
  then input_list |> List. tl |> parse_constructor_defn 
  else read_line () |> print_state "Type does not exist" |> read_input 


(** [main ()] prompts for the script to start, then starts it. *)
let main () =
  print_string "\n\nWelcome to the 3110 Database Command Line\n";
  print_endline "Please describe the data you want to store. The commands are of the form:\nTypeName id\ntype valueName\n...\nend";
  read_line () |> read_input |> ignore

(* Execute the game engine. *)
let () = main ()
