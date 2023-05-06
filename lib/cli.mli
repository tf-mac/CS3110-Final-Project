(*type input = Empty | Malformed | Valid of string list

  val user_defined_types : string list ref
  val lst_to_string : string list -> string
  val check_value_defn : string list -> input
  val parse_value_defn : string -> input
  val add_type : string -> unit
  val print_state : string -> string
  val read_value_defn : string -> string list list
  val parse_constructor_defn : string list -> string list list
  val read_make : string -> string -> string list
  val read_input : string -> unit*)
val parse_input : string -> string
val main : unit -> unit
