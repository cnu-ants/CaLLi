type t = Integer of {bitwidth:int}
        | Float
        | Pointer of {ty:t}
        | Vector of {size:int; ty:t}
        | Array of {size:int; ty:t}
        (* | Structure of {ty:t list} *)
        | Label
        | Void
        | Double
        | Undef

let rec get_bitwidth ty =
  match ty with
  | Integer {bitwidth} -> if bitwidth < 8 then 8 else bitwidth
  | Pointer _ -> 8
  | Vector {ty; _;} -> (get_bitwidth ty)
  | Array {ty; size;} -> (get_bitwidth ty) * size
  | _ -> failwith "Type.get_bitwidth : Not implemented yet"

let get_size ty = 
  match ty with
  | Vector {size; _;} -> size
  | Array {size; _;} -> size
  | _ -> failwith "Type.get_size : Theres no size for ty"


let rec pp ppf (ty : t) =
  match ty with
  | Integer {bitwidth} -> Format.fprintf ppf "i%d" bitwidth
  | Float -> Format.fprintf ppf "float"
  | Pointer {ty} -> Format.fprintf ppf "%a*" pp ty
  | Vector {size; ty} -> Format.fprintf ppf "<%d x %a>" size pp ty
  | Array {size; ty} -> Format.fprintf ppf "[%d x %a]" size pp ty
  | Label -> Format.fprintf ppf "label"
  | Void -> Format.fprintf ppf "void"
  | Double -> Format.fprintf ppf "double"
  | Undef -> Format.fprintf ppf "undef"

