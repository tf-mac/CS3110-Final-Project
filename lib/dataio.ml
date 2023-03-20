open Database

(* This code needs to be modularized to different implementations of a table. Currently the code assumes a ListOfTuplesTable implementation*)
let store databas file =
  let oc = Stdlib.open_out file in
  output_string oc (Database.db_to_string databas);
  Stdlib.flush oc

let read_db file = let oc = Stdlib.open_in file in
let rec rec_lines working_db working_table = let line = input_line oc in if line = "" then working_table :: working_db else 