type t = Br of {bb_name:string; succ:Expr.t}
        | CondBr of {bb_name:string; cond:Expr.t; succ0:Expr.t; succ1:Expr.t}
        | Switch of {bb_name:string; cond:Expr.t; default_succ:Expr.t;  succ: (Expr.t * Expr.t) list}
        | Ret of {bb_name:string; ret:Expr.t}
        | Exit of {bb_name:string}
        | CallSite of {bb_name:string; callee:string; args:Expr.t list}
        | Other

let pp_switch_succ fmt succ = 
  Format.fprintf fmt "\\l%a"
  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",\\l ")
  (fun fmt (e1, e2) -> Format.fprintf fmt "%a %a" Expr.pp e1 Expr.pp e2))
  succ

let pp ppf (term:t) =
  match term with
  | CallSite {callee; args; _} -> 
    let _ = Format.fprintf ppf "call %s(" callee in
    let _ =
      List.iter
      (fun arg -> Format.fprintf ppf "%a" Expr.pp arg)
      args
    in
    Format.fprintf ppf ")"
  | Br {succ; _} -> Format.fprintf ppf "br %a" Expr.pp succ
  | CondBr {cond; succ0; succ1; _} -> 
      Format.fprintf ppf "condbr %a %a %a" Expr.pp cond Expr.pp succ0 Expr.pp succ1
  | Ret {ret; _} -> Format.fprintf ppf "ret %a" Expr.pp ret
  | Exit _ -> Format.fprintf ppf "exit"
  | Switch {cond; default_succ; succ; _;} -> 
      Format.fprintf ppf "switch %a %a [%a]" Expr.pp cond Expr.pp default_succ pp_switch_succ succ
  | Other -> Format.fprintf ppf "other"

