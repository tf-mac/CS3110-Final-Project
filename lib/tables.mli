open Utils

exception IndexExists
exception TypeMismatch

module type Table = sig
  type t

  (* [empty ex] initializes an empty table [ex]*)
  val empty : entry list -> t

  (* Adds a new set of entries to the table. Precondition: Entry list has at least one element
     Raises [IndexExists] if the index provided is already in the table.
     Raises [TypeMismatch] if the new row fits the type definition*)
  val insert : t -> entry list -> t

  (* Adds a new set of entries to the table, in the columns that the ids identify. Any non-entered rows will be a none
     Raises [IndexExists] if the index provided is already in the table.
     Raises [TypeMismatch] if the new row fits the type definition*)
  val insert_named : t -> (string * entry) list -> t

  (* [at t id] returns a list of entry options representing all entries [id] in the [t]*)
  val at : t -> entry -> entry option list

  (* [delete t id] removes the entire row in [t] containing [id]. It returns the updated table.*)
  val delete : t -> entry -> t

  (* [table_to_string t]  returns a string representation of [t]*)
  val table_to_string : t -> string

  (* Returns the header of the table*)
  val header : t -> entry list

  (* Returns the type of the named column, if it exists. If not, raises [TypeMismatch]*)
  val exists : t -> string -> types
end

module ListTable : Table
module HashTable : Table
