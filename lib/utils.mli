(** This module holds utilities (both functions and types) that many other modules need.*)

(** Representation of the type of some field*)
type types = Strings | Floats | Ints | Chars | Bools | Ids

(** Representation of some comparison operation*)
type comparison = LT | LTE | EQ | NEQ | GT | GTE

exception IndexExists
(** [IndexExists] occurs when trying to replace an index which already exists*)

exception TypeMismatch
(** [TypeMismatch] occurs when some typing error occurs, e.g. true > "cat"*)

(** The core of this project! The type holding every entry in every table*)
type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * types)

val process_entry : string -> types -> entry
(** Turns a string into the given entry type.
   Raises [TypeMismatch] if this cannot occur*)

val run_constraint : comparison -> entry -> entry -> bool
(**[run_constraint cmp rhs lhs] returns a bool equal to lhs (cmp) rhs
       Raises [TypeMismatch] if type of lhs != rhs*)

val name_map_types : types -> string
(** [name_map_types t] maps each [t] to a string.*)

val entry_to_string : entry -> string
(**[entry_to_string entry] converts [entry] into a string*)

val shorten : string -> string
(** [shorten inp] shortens [inp] to a maximum of 16 characters*)

val build_row : entry option list -> string
(** [build_row entlist] converts [entlist] into a string, using shorten to limit the length of each entry.*)

val optionize : entry list -> entry option list
(** [optionize] wraps them in Some constructors*)
