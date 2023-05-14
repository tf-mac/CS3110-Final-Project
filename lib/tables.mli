open Utils

exception IndexExists
exception TypeMismatch

module type Table = sig
  type t

  val empty : entry list -> t
  (** [empty lst] Creates a new table given a list of type definitions. Raises [TypeMismatch] if the list provided is not types**)

  val insert : t -> entry list -> t
  (** Adds a new set of entries to the table. Precondition: Entry list has at least one element
     Raises [IndexExists] if the index provided is already in the table.
     Raises [TypeMismatch] if the new row fits the type definition*)

  (* Adds a new set of entries to the table, in the columns that the ids identify. Any non-entered rows will be a none
     Raises [IndexExists] if the index provided is already in the table.
     Raises [TypeMismatch] if the new row fits the type definition*)
  val insert_named : t -> (string * entry) list -> t
  val at : t -> entry -> entry option list
  val delete : t -> entry -> t
  val table_to_string : t -> string

  (* Processes a given list of constraints
     Raises [Not_found] if a constraint isn't found
     Raises [TypeMismatch] if the comparison value doesn't match the header*)
  val process_constraints : t -> (string * comparison * string) list -> string

  (* Returns the header of the table*)
  val header : t -> entry list

  (* Returns the type of the named column, if it exists. If not, raises [TypeMismatch]*)
  val exists : t -> string -> types
end

module ListTable : Table
module HashTable : Table
