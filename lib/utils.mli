type types = Strings | Floats | Ints | Chars | Bools | Ids
type comparison = LT | LTE | EQ | NEQ | GT | GTE

exception IndexExists
exception TypeMismatch

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * types)

(*[name_map_entry t] returns a string representing the corresponding data type [t]*)
val name_map_entry : entry -> string

(* Turns a string into the given entry type.
   Raise [TypeMismatch] if this cannot occur*)
val process_entry : string -> types -> entry
val run_constraint : comparison -> entry -> entry -> bool

(* [name_map_types t] maps each [t] to a string.*)

val name_map_types : types -> string

(*[entry_to_string entry] converts [entry] into a string*)
val entry_to_string : entry -> string

(* [shorten inp] shortens [inp] to a maximum of 16 characters*)
val shorten : string -> string

(* [build_row entlist] converts [entlist] into a string, using shorten to limit the length of each entry.*)
val build_row : entry option list -> string

(* [optionize] wraps them in Some constructors*)
val optionize : entry list -> entry option list
