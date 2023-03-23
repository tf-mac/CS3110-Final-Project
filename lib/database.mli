module type Table = sig

  type t
  (** [t] represents is a table that hold information *)
  type value
    (** [value] represents a key value associated with an index in a table *)

  val empty : value -> t
    (** [empty ex] is the empty table associated with value [ex]*)
  val insert : t -> value -> t
    (** [insert t id] is the same table as [t] but with an additional entry with 
    value [id]
      *)
  val at : t -> value -> value
    (** [at t id] is the value in [t] at index [id]
      *)
  val delete : t -> value -> t
    (** [delete t id] deletes the value in [t] at index [id]
      *)
  val table_to_string : t -> string
    (** [table_to_string t] gives the string represention of [t] in a tabular format
      *)
end

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * entry)
(** [entry] represents a type which can hold all OCaml primitives *)

val entry_to_string : entry -> string
(** [entry_to_string ent] gives the string representation of [ent]*)

module ListOfTupleTable : sig
  type t = entry list list
  (** [t] represents is a table that hold information *)

  type value = entry list
  (** [value] represents a key value associated with an index in a table *)

  val empty : value -> t
  (** [empty ex] is the empty table associated with value [ex]*)
  val insert : t -> value -> t
  (** [insert t id] is the same table as [t] but with an additional entry with 
    value [id]*)
  val at : t -> value -> value
      (** [at t id] is the value in [t] at index [id]*)
  val delete : t -> value -> t
      (** [delete t id] deletes the value in [t] at index [id] *)
  val table_to_string : t -> string
      (** [table_to_string t] gives the string represention of [t] in a 
          tabular format*)
end

module Database : sig
  exception NoEntry
  (** Raised when an entry that does not exist is parsed. *)
  exception WrongType
  (** Raised when an entry with in incorrect type is parsed *)

  module T = ListOfTupleTable

  val empty : 'a list
  (** [empty] is a table containing no elements
      *)
  val add_table : (string *T.t) list -> entry list -> string -> (string *T.t) list
  (** [add_table database id name] adds an empty table to [database]
      Raises: [Bad] when the value associated with [name] does not have the 
      same type as [id] 
      *)
  val drop_table : string -> (string * 'b) list -> (string * 'b) list
  (** [drop_table name database] deletes a table with value [name] from [database]
      *)
  val get_table : string -> (string * 'b) list -> string * 'b
  (** [get_table name database] is the tabe with value [name] in [database]
      *)
  val get_reference : entry -> (string *T.t) list -> T.value
  (** [get_reference ent database] does not work as intended now
      *)
  val db_to_string : (string *T.t) list -> string
  (** [db_to_string database] gives the string representation of [database]
      *)
  val check_value : (string *T.t) list -> string -> string -> string -> unit
  (** [check_value database tn eid ev] checks whether [tn] [eid] [ev] exists in 
    [database] (this is totally wrong. We need to clean up this method)
      *)
  val add_entry : string -> string list -> (string *T.t) list -> (string *T.t) list
  (** [add_entry table_name new_row database] is the same [database] with a new 
  entry [new_row] associated with value [table_name] 
      *)
  val process_new_types : string list list  -> entry list
  (** [process_new_types types inputs] matches user string [inputs] with OCaml
      primitive types [types]
      *)
end
