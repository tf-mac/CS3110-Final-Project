type types = Strings | Floats | Ints | Chars | Bools | Ids
type comparison = LT | LTE | EQ | NEQ | GT | GTE

exception IndexExists
exception TypeMismatch

type entry =
  | String of string
  | Float of float
  | Int of int
  | Char of char
  | Bool of bool
  | Id of (string * entry)
  | Type of (string * types)

let name_map_entry t =
  match t with
  | String _ -> "string"
  | Float _ -> "float"
  | Int _ -> "int"
  | Char _ -> "char"
  | Bool _ -> "bool"
  | Id _ -> "id"
  | Type _ -> "type"

let name_map_types t =
  match t with
  | Strings -> "string"
  | Floats -> "float"
  | Ints -> "int"
  | Chars -> "char"
  | Bools -> "bool"
  | Ids -> "id"

let guess_entry input =
  match float_of_string_opt input with
  | Some v -> Float v
  | None -> (
      match int_of_string_opt input with
      | Some v -> Int v
      | None -> (
          match bool_of_string_opt input with
          | Some v -> Bool v
          | None ->
              if String.length input = 1 then Char input.[0] else String input))

let process_entry input = function
  | Strings -> String input
  | Floats -> (
      match float_of_string_opt input with
      | None -> raise TypeMismatch
      | Some v -> Float v)
  | Ints -> (
      match int_of_string_opt input with
      | None -> raise TypeMismatch
      | Some v -> Int v)
  | Chars ->
      if String.length input = 1 then Char input.[0] else raise TypeMismatch
  | Bools -> (
      match bool_of_string_opt input with
      | None -> raise TypeMismatch
      | Some v -> Bool v)
  | Ids -> (
      match String.split_on_char '@' input with
      | [] | [ _ ] | _ :: _ :: _ :: _ -> raise TypeMismatch
      | [ hd; tl ] -> Id (hd, guess_entry tl))

let run_constraint cmp rhs lhs =
  match cmp with
  | LT -> (
      match lhs with
      | Float lhs -> (
          match rhs with Float rhs -> lhs < rhs | _ -> raise TypeMismatch)
      | _ -> raise TypeMismatch)
  | _ -> failwith "Unimplemented"

let rec entry_to_string ent =
  match ent with
  | String x -> x
  | Float x -> string_of_float x
  | Int x -> string_of_int x
  | Char x -> String.make 1 x
  | Bool x -> string_of_bool x
  | Id (a, b) -> a ^ "@" ^ entry_to_string b
  | Type (a, b) -> name_map_types b ^ " " ^ a

let shorten inp =
  let str = String.trim inp in
  if String.length str < 8 then str ^ "\t\t"
  else if String.length str < 16 then str ^ "\t"
  else String.sub str 0 16

let rec build_row entlist =
  match entlist with
  | [] -> "\n\n"
  | Some x :: xs -> shorten (entry_to_string x) ^ build_row xs
  | None :: xs -> "\t\t" ^ build_row xs

let rec optionize = function [] -> [] | hd :: tl -> Some hd :: optionize tl
