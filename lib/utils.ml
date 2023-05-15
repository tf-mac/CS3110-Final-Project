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

let name_map_types t =
  match t with
  | Strings -> "string"
  | Floats -> "float"
  | Ints -> "int"
  | Chars -> "char"
  | Bools -> "bool"
  | Ids -> "id"

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
  | _ -> raise TypeMismatch

let make_compare cmp lhs rhs =
  match cmp with
  | LT -> lhs < rhs
  | LTE -> lhs <= rhs
  | EQ -> lhs = rhs
  | NEQ -> lhs <> rhs
  | GT -> lhs > rhs
  | GTE -> lhs >= rhs

let run_constraint (cmp : comparison) rhs lhs =
  match rhs with
  | Float r -> (
      match lhs with
      | Float l -> make_compare cmp l r
      | _ -> failwith "Typing error")
  | Int r -> (
      match lhs with
      | Int l -> make_compare cmp l r
      | _ -> failwith "Typing error")
  | Char r -> (
      match lhs with
      | Char l -> make_compare cmp l r
      | _ -> failwith "Typing error")
  | Bool r -> (
      match lhs with
      | Bool l -> make_compare cmp l r
      | _ -> failwith "Typing error")
  | String r -> (
      match lhs with
      | String l -> make_compare cmp l r
      | _ -> failwith "Typing error")
  | _ -> raise TypeMismatch

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
