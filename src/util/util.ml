let get_name lli =
  try
  Str.global_replace (Str.regexp "[\r\n\t ]") "" (List.hd (String.split_on_char '=' (Llvm.string_of_llvalue lli)))
  with | _ -> failwith "get_name err"

let get_bbname ?(default="entry") lli =
match String.split_on_char ':' (Llvm.string_of_llvalue lli) with
| [] -> failwith "Unreachable"
| _ :: [] -> default
| h :: _ -> Str.global_replace (Str.regexp "[\r\n\t ]") "" h

let get_llbb_name llbb =
  (get_bbname (Llvm.value_of_block llbb))

let get_vec_type e =
  let n = (Str.global_replace (Str.regexp "[\r\n\t <>]") "" (List.nth (String.split_on_char ' ' (Llvm.string_of_llvalue e)) 2)) in
  match n with
  | "i32" -> Type.Integer {bitwidth=32}
  | "i1" -> Type.Integer {bitwidth=1}
  | _ -> failwith "Not implemented"

let get_vec_list e = 
  let str = (Str.global_replace (Str.regexp "[<>]") "" (List.nth (String.split_on_char '<' (Llvm.string_of_llvalue e)) 2)) in
  let vec = (String.split_on_char ',' str) in
  List.fold_left
  (fun vector v -> vector@[(String.trim v)])
  []
  vec

let get_list_list e = 
  let str = (Str.global_replace (Str.regexp "]") "" (List.nth (String.split_on_char '[' (Llvm.string_of_llvalue e)) 2)) in
  let vec = (String.split_on_char ',' str) in
  List.fold_left
  (fun vector v -> vector@[(String.trim v)])
  []
  vec

let get_array_list e =
  let expr = Llvm.operand e 1 in
  let _ = Format.printf "%s" (Llvm.string_of_llvalue expr) in  ()

let get_type e = 
  let n = (Str.global_replace (Str.regexp "[\r\n\t ]") "" (List.nth (String.split_on_char ' ' e) 0)) in
  match n with
  | "i32" -> Type.Integer {bitwidth=32}
  | "i1" -> Type.Integer {bitwidth=1}
  | "float" -> Type.Float 
  | "double" -> Type.Double
  | _ -> failwith "type error"

let get_int e : Z.t = 
  let n = (Str.global_replace (Str.regexp "[\r\n\t ]") "" (List.nth (String.split_on_char ' ' e) 1)) in
  match n with
  | "true" -> Z.of_int 1
  | "false" -> Z.of_int 0
  | _ -> Z.of_string n

let get_float e = 
  let f = (Str.global_replace (Str.regexp "[\r\n\t ]") "" (List.nth (String.split_on_char ' ' e) 1)) in
  try 
  (match f with
  | _ -> float_of_string f
  ) with _ -> let _ = Format.printf "%s@." f in failwith "util.get_float"

let get_fname func =
  try 
  let ret = Str.global_replace (Str.regexp "[\r\n\t ]") "" (List.hd (String.split_on_char ')' (List.hd (List.tl (String.split_on_char '@' (Llvm.string_of_llvalue func)))))) in
  ret ^ (String.make 1 ')')
  with | _ -> let _ = Format.printf "%s@." (Llvm.string_of_llvalue func)  in "unknown"

let get_fname_from_bb str =
  try
  Str.global_replace (Str.regexp "[\r\n\t ]") "" (List.hd (String.split_on_char '#' str))
  with | _ -> failwith "get fname from bb err"  

let is_global str = 
  String.starts_with ~prefix:"@" str
