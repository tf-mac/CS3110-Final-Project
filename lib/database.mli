(** This module handles the complete backend for data storage. It is able to handle multiple types
    and build tables accordingly, data can be accessed and added to any type in various ways*)

open Utils
open Tables

module Database (Table : Table) : sig
  exception NoEntry
  (** Raised when an entry that does not exist is parsed. *)

  exception TableExists
  (** Raised when a table is attempted to be added which already exists*)

  type table = Table.t
  (** Internal table of the database*)

  type database
  (** Internal database representation*)

  val empty : database
  (** [empty] is a table containing no elements*)

  val add_table : database -> table -> string -> database
  (** [add_table database table name] adds table to [database]
      Raises: [IndexExists] when the value associated with [name] exists
      *)

  val build_table : database -> entry list -> string -> database
  (** [build_table database lst name] builds a new table inside [database] using [lst] as a header
      Raises [IndexExists] if the name is already in use*)

  val drop_table : string -> database -> database
  (** [drop_table name database] returns a database with the table [name] removed*)

  val get_table : string -> database -> table option
  (** [get_table name database] returns table option containing table [name], if it exists in [database]*)

  val get_reference : entry -> database -> entry list * entry option list option
  (** [get_reference id database] returns a tuple containing the header of the table [id] refers to and the row in the table it describes, if that row exists
      Raises [Not_found] if the table referenced doesn't exist
      Raises [TypeMismatch] if id is not an entry Id*)

  val db_to_string : database -> string
  (** [db_to_string database] returns a string representation of database*)

  val add_entry : string -> entry list -> database -> unit
  (** [add_entry s lst db] Adds an entry to the table given by id. Raises [Not_found] if the table cannot be found*)

  val add_named_entry : string -> (string * entry) list -> database -> unit
  (** [add_named_entry table lst database] adds the lst to the table specified, where the list is tuples containing the name of the field wishing to be filled and the entry to fill it with
      Raises [Not_found] if the table does not exist
      Raises [TypeMismatch] if any of the entries do not match the type of their fields*)
end
