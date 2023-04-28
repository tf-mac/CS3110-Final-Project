open Utils

exception IndexExists
exception TypeMismatch

module type Table = sig
  type t

  (* Creates a new table given a list of type definitions. Raises [TypeMismatch] if the list provided is not types*)
  val empty : entry list -> t

  (* Adds a new set of entries to the table. Precondition: Entry list has at least one element
     Raises [IndexExists] if the index provided is already in the table.
     Raises [TypeMismatch] if the new row fits the type definition*)
  val insert : t -> entry list -> t

  (* Adds a new set of entries to the table, in the columns that the ids identify. Any non-entered rows will be a none
     Raises [IndexExists] if the index provided is already in the table.
     Raises [TypeMismatch] if the new row fits the type definition*)
  val insert_named : t -> (string * entry) list -> t
  val at : t -> entry -> entry option list
  val delete : t -> entry -> t
  val table_to_string : t -> string

  (* Returns the header of the table*)
  val header : t -> entry list

  (* Returns the type of the named column, if it exists. If not, raises [TypeMismatch]*)
  val exists : t -> string -> types
end

module ListTable : Table
module HashTable : Table
