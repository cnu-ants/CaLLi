type t = FNeg | Add | FAdd | Sub | FSub | Mul | FMul 
        | SDiv | UDiv | FDiv | SRem | URem | FRem | Shl
        | LShr | AShr | And | Or | Xor

let to_string op = 
  match op with
  | FNeg -> "-." 
  | Add -> "+"
  | FAdd -> "+."
  | Sub -> "-"
  | FSub -> "-."
  | Mul -> "*"
  | FMul -> "*."
  | SDiv -> "sdiv"
  | UDiv -> "udiv"
  | FDiv -> "/."
  | SRem -> "srem"
  | URem -> "urem"
  | FRem -> "%."
  | Shl -> "<<"
  | LShr -> "lshr"
  | AShr -> "ashr"
  | And -> "&"
  | Or -> "|"
  | Xor -> "xor"

let pp fmt op = 
  Format.fprintf fmt "%s" (to_string op)
