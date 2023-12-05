
type typed_var = {ty:Type.t; name:string}

type t = ConstInt of {ty:Type.t; value:Z.t}
          | ConstFP of {ty:Type.t; value:float}
          | Name of typed_var
          | Vector of {ty:Type.t; value:t list}
          | Array of {ty:Type.t; value:t list}
          | Void of {ty:Type.t}
          | Undef
          (* | Structure of {ty:Type.t; value:t list} *)

let typed_var_of_expr expr = 
  match expr with
  | Name typed_var -> typed_var
  | _ -> failwith "typed_var_of_expr : Not a Name"

let get_type expr =
  match expr with
  | ConstInt {ty; _;} -> ty
  | ConstFP {ty; _;} -> ty
  | Vector {ty; _;} -> ty
  | Name typed_var -> typed_var.ty
  | Array {ty; _;} -> ty
  | Void {ty} -> ty
  | Undef -> failwith "get_type undef"

let rec pp ppf (expr : t) =
  match expr with
  | ConstInt {ty; value} -> Format.fprintf ppf "%a %s" Type.pp ty (Z.to_string value)
  | ConstFP {ty; value} -> Format.fprintf ppf "%a %f" Type.pp ty value
  | Name {ty; name} -> Format.fprintf ppf "%a %s" Type.pp ty name
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

