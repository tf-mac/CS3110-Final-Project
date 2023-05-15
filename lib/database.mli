open Utils
open Tables

module Database (Table : Table) : sig
  (*[NoEntry] is raised when an entry cannot be found in a table*)
  exception NoEntry

  (*[WrongType] is raised when a value of the wrong type is encountered*)
  exception WrongType

  (*[TableExists] is raised when a table with the same name already exists in the database.*)
  exception TableExists

  type table = Table.t
  type database

  (*empty is the empty database*)
  val empty : database

  (* [add_table database table name] adds [table] to [database] with the given name [name].
     If a table with the same name already exists in the database, [TableExists] is raised *)
  val add_table : database -> table -> string -> database

  (* [build_table database table name] creates [table] with the given name [name] in [database].
     If a table with the same name already exists in the database, [TableExists] is raised*)
  val build_table : database -> entry list -> string -> database

  (* [drop_table name database] removes the table with [name] from [database]*)
  val drop_table : string -> database -> database

  (* [get_table name database] returns the table with [name] if it exists [database] or [None] if it does not*)
  val get_table : string -> database -> table option

  (* [get_reference ent database]returns a reference to [ent] if it exists in [database] or raises [NoEntry] if it does not*)
  val get_reference : entry -> database -> string * table

  (* [db_to_string database] returns a string representation of [database]*)
  val db_to_string : database -> string

  (*[add_entry name new_row database]  adds [new_row] to the table with [name] in [database]*)
  val add_entry : string -> entry list -> database -> unit

  (*[add_named_entry name new_row database] adds [new_row] to the table with [name] in [database]*)

  val add_named_entry : string -> (string * entry) list -> database -> unit
end
