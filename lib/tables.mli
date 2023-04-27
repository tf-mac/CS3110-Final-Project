open Utils

module type Table = sig
  type t

  exception IndexExists
  exception TypeMismatch

  (* Creates a new table given a list of type definitions. Raises [TypeMismatch] if the list provided is not types*)
  val empty : entry list -> t

  (* Adds a new set of entries to the table. Raises [IndexExists] if the index provided is already in the table*)
  val insert : t -> entry list -> t
  val at : t -> entry -> entry list
  val delete : t -> entry -> t
  val table_to_string : t -> string

  (* Returns the header of the table*)
  val header : t -> entry list
end

module ListTable : Table
