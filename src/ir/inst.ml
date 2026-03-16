type prune_ty =
  | Value of { lhs : string; rhs : Expr.t }
  | Predicate of { cond : Cond.t; operand0 : Expr.t; operand1 : Expr.t }
  | Pointer of { lhs : string; operand : Expr.t }

type t = BinaryOp of {name:string; op:Op.t; operand0:Expr.t; operand1:Expr.t; ty:Type.t;}
            | Alloc of {name:string; ty:Type.t} 
            | Store of {operand:Expr.t; name:string; ty:Type.t}
            | Load of {name:string; operand:Expr.t; ty:Type.t}
            | PtrToInt of {name:string; operand:Expr.t; ty:Type.t}
            | IntToPtr of {name:string; operand:Expr.t; ty:Type.t}
            | ICmp of {name:string; cond:Cond.t; operand0:Expr.t; operand1:Expr.t; ty:Type.t}
            | Select of {name:string; cond:Expr.t; operand0:Expr.t; operand1:Expr.t; ty:Type.t}
            | ReturnSite of {name:string; ty:Type.t}
            | Call of {name:string; callee:string; args:Expr.t list; ty:Type.t}
            | GetElementPtr of {name:string; ty:Type.t; operand:Expr.t; index:Expr.t list}
            | BitCast of {name:string; operand:Expr.t; ty:Type.t;}
            | Sext of {name:string; operand:Expr.t; ty:Type.t}
            | Zext of {name:string; operand:Expr.t; ty:Type.t}
            | Prune of prune_ty
            | NPrune of {cond:string; value:Expr.t list}
            | Trunc of {name:string; operand:Expr.t; ty:Type.t}
            | Other

let pp_prune_ty ppf (pr : prune_ty) =
  match pr with
  | Value { lhs; rhs } ->
      Format.fprintf ppf "%s = %a" lhs Expr.pp rhs
  | Predicate { cond; operand0; operand1 } ->
      Format.fprintf ppf "%a %a %a" Expr.pp operand0 Cond.pp cond Expr.pp operand1
  | Pointer { lhs; operand } ->
      Format.fprintf ppf "%s = *%a" lhs Expr.pp operand

let pp ppf (inst : t) =
  match inst with
  | BinaryOp { name; op; operand0; operand1; _ } ->
      Format.fprintf ppf "%s = %a %a %a" name Op.pp op Expr.pp operand0 Expr.pp operand1
  | Alloc { name; ty } ->
      Format.fprintf ppf "%s = alloc %a" name Type.pp ty
  | Store { operand; name; ty } ->
      Format.fprintf ppf "store %a %a %s" Expr.pp operand Type.pp ty name
  | Load { name; operand; _ } ->
      Format.fprintf ppf "%s = load %a" name Expr.pp operand
  | PtrToInt { name; operand; ty } ->
      Format.fprintf ppf "%s = ptrtoint %a to %a" name Expr.pp operand Type.pp ty
  | IntToPtr { name; operand; ty } ->
      Format.fprintf ppf "%s = inttoptr %a to %a" name Expr.pp operand Type.pp ty
  | ICmp { name; cond; operand0; operand1; _ } ->
      Format.fprintf ppf "%s = icmp %a %a %a" name Cond.pp cond Expr.pp operand0 Expr.pp operand1
  | Select { name; cond; operand0; operand1; _ } ->
      Format.fprintf ppf "%s = select %a %a %a" name Expr.pp cond Expr.pp operand0 Expr.pp operand1
  | ReturnSite { name; ty } ->
      Format.fprintf ppf "return site %s = %a" name Type.pp ty
  | GetElementPtr { name; ty; operand; index } ->
      let _ = Format.fprintf ppf "%s = getelementptr %a %a" name Type.pp ty Expr.pp operand in
      List.iter (fun idx -> Format.fprintf ppf ", %a" Expr.pp idx) index
  | BitCast { name; operand; ty } ->
      Format.fprintf ppf "%s = bitcast %a to %a" name Expr.pp operand Type.pp ty
  | Zext { name; operand; ty } ->
      Format.fprintf ppf "%s = zext %a to %a" name Expr.pp operand Type.pp ty
  | Sext { name; operand; ty } ->
      Format.fprintf ppf "%s = sext %a to %a" name Expr.pp operand Type.pp ty
  | Call { name; _ } ->
      Format.fprintf ppf "call %s" name
  | Prune pr ->
      Format.fprintf ppf "prune %a" pp_prune_ty pr
  | NPrune { cond; value } ->
      Format.fprintf ppf "!prune %s %a" cond
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt a -> Format.fprintf fmt "%a" Expr.pp a))
        value
  | Trunc { name; operand; ty } ->
      Format.fprintf ppf "%s = trunc %a to %a" name Expr.pp operand Type.pp ty
  | Other ->
      Format.fprintf ppf "Other"
