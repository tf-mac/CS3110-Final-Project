module type Table = sig
  type t
  type value

  val empty : value -> t
  val insert : t -> value -> t
  val at : t -> value -> value
  val delete : t -> value -> t
  val table_to_string : t -> string
end

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * entry)

val entry_to_string : entry -> string

module ListOfTupleTable : sig
  type t = entry list list
  type value = entry list

  val empty : value -> t
  val insert : t -> value -> t
  val at : t -> value -> value
  val delete : t -> value -> t
  val table_to_string : t -> string
end

module Database : sig
  module T = ListOfTupleTable

  val empty : 'a list
  val add_table : ('a * T.t) list -> entry list -> 'a -> ('a * T.t) list
  val drop_table : 'a -> ('a * 'b) list -> ('a * 'b) list
  val get_table : 'a -> ('a * 'b) list -> 'a * 'b
  val get_reference : entry -> (string * T.t) list -> T.value
  val db_to_string : (string * T.t) list -> string
end
