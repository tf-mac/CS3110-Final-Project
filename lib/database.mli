open Utils
open Tables

module Database (Table : Table) : sig
  exception NoEntry
  (** Raised when an entry that does not exist is parsed. *)
  exception WrongType
  exception TableExists

  type table = Table.t
  type database

  val empty : database
  val add_table : database -> table -> string -> database
  val build_table : database -> entry list -> string -> database
  val drop_table : string -> database -> database
  val get_table : string -> database -> table option
  val get_reference : entry -> database -> (entry list * entry option list option)
  val db_to_string : database -> string
  val db_to_file : database -> string
  val db_of_file : string -> database

  (* Adds an entry to the table given by id. Raises [Not_found] if the table cannot be found*)
  val add_entry : string -> entry list -> database -> unit
  val add_named_entry : string -> (string * entry) list -> database -> unit
end
