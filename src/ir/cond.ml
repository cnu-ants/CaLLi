type t = Eq | Ne | Ugt | Ult | Sgt | Sge | Slt | Sle  | Uge | Ule

let to_string cond = 
  match cond with
  | Eq -> "eq"
  | Ne -> "ne"
  | Ugt -> "ugt"
  | Uge -> "uge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Slt -> "slt"
  | Sle -> "sle"

let pp fmt (cond : t) =
  Format.fprintf fmt "%s" (to_string cond)
