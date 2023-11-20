type t = Eq | Ne | Ugt | Ult | Sgt | Sge | Slt | Sle  | Uge | Ule

let to_string cond = 
  match cond with
  | Eq -> "eq"
  | Ne -> "ne"
  | Ugt -> "ugt"
  | Uge -> "uge"
  | Ult -> "ult"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Slt -> "slt"
  | Sle -> "sle"
  | Ule -> "ule"

let pp fmt (cond : t) =
  Format.fprintf fmt "%s" (to_string cond)
