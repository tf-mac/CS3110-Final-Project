module type Table = sig
  type t
  type value

  val empty : value -> t
  val insert : t -> value -> t
  val at : t -> int -> value
  val delete : t -> int -> t
  val table_to_string : t -> string
end

module type Database = sig
  module T : Table

  type entry = T.value

  val empty : 'a list
  val add_table : ('a * T.t) list -> entry list -> 'a -> ('a * T.t) list
  val drop_table : 'a -> ('a * 'b) list -> ('a * 'b) list
  val get_table : 'a -> ('a * 'b) list -> 'a * 'b
  val get_reference : entry -> (string * T.t) list -> T.value
  val db_to_string : (string * T.t) list -> string
end
