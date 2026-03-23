
type t = 
  | Null
  | NullPtr
  | BasicBlock of {name : string}
  | InlineAsm
  | BlockAddr
  | ConstExpr
  | ConstInt of {ty:Type.t; value:Z.t}
  | ConstFP of {ty:Type.t; value:float}
  | Function
  | GlobalVar of {ty:Type.t; name:string}
  | Poison
  | Vector of {ty:Type.t; value:t list}
  | Array of {ty:Type.t; value:t list}
  | Void of {ty:Type.t}
  | Undef
  | Var of {ty:Type.t; name:string; arg:bool}
          (* | Structure of {ty:Type.t; value:t list} *)


let get_type expr =
  match expr with
  | ConstInt {ty; _;} -> ty
  | ConstFP {ty; _;} -> ty
  | Vector {ty; _;} -> ty
  | Var {ty; _;} -> ty
  | Array {ty; _;} -> ty
  | Void {ty} -> ty
  | Undef -> failwith "get_type undef"
  | _ -> failwith "No type"

let get_name expr =
  match expr with
  | Var {name; _} -> name
  | GlobalVar {name; _} -> name
  | BasicBlock {name; _} -> name
  | _ -> failwith "Expr has no name"

let rec pp ppf (expr : t) =
  match expr with
  | Null -> Format.fprintf ppf "Null"
  | NullPtr -> Format.fprintf ppf "NullPtr"
  | BasicBlock {name} -> Format.fprintf ppf "%s" name
  | InlineAsm -> Format.fprintf ppf "InlineAsm"
  | BlockAddr -> Format.fprintf ppf "BlockAddr"
  | ConstExpr -> Format.fprintf ppf "ConstExpr"
  | ConstInt {ty; value} -> Format.fprintf ppf "%a %s" Type.pp ty (Z.to_string value)
  | ConstFP {ty; value} -> Format.fprintf ppf "%a %f" Type.pp ty value
  | Function -> Format.fprintf ppf "Function"
  | GlobalVar {ty; name} -> Format.fprintf ppf "%a %s" Type.pp ty name
  | Poison -> Format.fprintf ppf "Poison"
  | Vector {ty; value} -> 
    let _ = Format.fprintf ppf "%a <" Type.pp ty in
    let _ = List.iter
      (fun v -> Format.fprintf ppf "%a," pp v)
      value
    in
    Format.fprintf ppf ">"
  | Array {ty; value} ->
    let _ = Format.fprintf ppf "%a [" Type.pp ty in
    let _ = List.iter
      (fun v -> Format.fprintf ppf "%a," pp v)
      value
    in
    Format.fprintf ppf "]"
  | Void _ -> Format.fprintf ppf "void" 
  | Undef -> Format.fprintf ppf "undef" 
  | Var {ty; name; _} -> Format.fprintf ppf "%a %s" Type.pp ty name
