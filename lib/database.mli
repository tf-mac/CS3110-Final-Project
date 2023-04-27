open Utils
open Tables

module Database (Table : Table) : sig
  exception NoEntry
  exception WrongType
  exception TableExists

  type table
  type database

  val empty : database
  val add_table : database -> table -> string -> database
  val build_table : database -> entry list -> string -> database
  val drop_table : string -> database -> database
  val get_table : string -> database -> table option
  val get_reference : entry -> database -> string * table
  val db_to_string : database -> string
  val check_value : database -> 'a -> string -> string -> unit

  (* Adds an entry to the table given by id. Raises [Not_found] if the table cannot be found*)
  val add_entry : string -> entry list -> database -> database
  val process_new_types : string list list -> entry list
end
