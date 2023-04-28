open Utils
open Tables

module Database (Table : Table) : sig
  exception NoEntry
  exception WrongType
  exception TableExists

  type table = Table.t
  type database

  val empty : database
  val add_table : database -> table -> string -> database
  val build_table : database -> entry list -> string -> database
  val drop_table : string -> database -> database
  val get_table : string -> database -> table option
  val get_reference : entry -> database -> string * table
  val db_to_string : database -> string

  (* Adds an entry to the table given by id. Raises [Not_found] if the table cannot be found*)
  val add_entry : string -> entry list -> database -> unit
  val add_named_entry : string -> (string * entry) list -> database -> unit
end
