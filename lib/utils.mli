type types = Strings | Floats | Ints | Chars | Bools | Ids
type comparison = LT | LTE | EQ | NEQ | GT | GTE

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * types)

val name_map_entry : entry -> string
val name_map_types : types -> string
val entry_to_string : entry -> string
val shorten : string -> string
val build_row : entry option list -> string
val optionize : entry list -> entry option list
