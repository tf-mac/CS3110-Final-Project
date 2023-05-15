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

val main : unit -> unit
(** [main ()] parses the user's input from the terminal using 
    CliHandler and prints the resulting message to the terminal until the user quits *)

module type CliHandler = sig
  val parse_input : string -> string
  (**[parse_input input] takes the string [input] and uses the internal state to produce an output string, which is equivalent to what would be printed in a CLI. Modifies internal state accordingly*)

  val get_response : string -> string
  (**[get_response addr] reads the responses json, giving a response string for a response string
      Raises [Not_found] if [addr] is an invalid response*)

  val reset : unit -> unit
  (**[reset] changes the internal state to have an empty database and default internal state
          *)
end

module CLI : CliHandler
