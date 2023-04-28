type types = Strings | Floats | Ints | Chars | Bools | Ids

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * types)

let name_map_entry t =
  match t with
  | String _ -> "string"
  | Float _ -> "float"
  | Int _ -> "int"
  | Char _ -> "char"
  | Bool _ -> "bool"
  | Id _ -> "id"
  | Type _ -> "type"

let name_map_types t =
  match t with
  | Strings -> "string"
  | Floats -> "float"
  | Ints -> "int"
  | Chars -> "char"
  | Bools -> "bool"
  | Ids -> "id"

let rec entry_to_string ent =
  match ent with
  | String x -> x
  | Float x -> string_of_float x
  | Int x -> string_of_int x
  | Char x -> String.make 1 x
  | Bool x -> string_of_bool x
  | Id (a, b) -> a ^ "@" ^ entry_to_string b
  | Type (a, b) -> name_map_types b ^ " " ^ a

let shorten inp =
  let str = String.trim inp in
  if String.length str < 8 then str ^ "\t\t"
  else if String.length str < 16 then str ^ "\t"
  else String.sub str 0 16
