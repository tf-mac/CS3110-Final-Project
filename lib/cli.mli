(** This module handles the command line interface, it allows easy use of the backend infrastructure
    for storing and retrieving new types and data.*)

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
