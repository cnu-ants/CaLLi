module Alias = struct
  module M = Map.Make(String)
  type aliasty = 
    Pointer of Expr.t | Predicate of {cond:Cond.t; operand0:Expr.t; operand1:Expr.t}
  type t = aliasty M.t
  let empty = M.empty
  let add = M.add
  let find = M.find
  let find_opt = M.find_opt

  let pp_aliasty fmt (k,v) =
    match v with
    | Pointer e -> Format.fprintf fmt "%s<->*(%a) " k Expr.pp e
    | Predicate {cond; operand0; operand1} -> 
      Format.fprintf fmt "%s<->(%a %a %a) " k Cond.pp cond Expr.pp operand0 Expr.pp operand1
         

  let pp fmt a = 
    let _ = Format.printf "alias : " in
    M.iter 
    (fun k v ->
    match v with
    | Pointer e -> Format.fprintf fmt "%s<->*(%a) " k Expr.pp e
    | Predicate {cond; operand0; operand1} -> 
      Format.fprintf fmt "%s<->(%a %a %a) " k Cond.pp cond Expr.pp operand0 Expr.pp operand1
    )
    a
end
    
type t = Empty | Meta of {alias:Alias.t}

let pp fmt meta = 
  match meta with
  | Empty -> Format.printf "empty metadata"
  | Meta {alias} -> Format.fprintf fmt "%a" Alias.pp alias

