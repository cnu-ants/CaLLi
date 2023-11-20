


type t = BinaryOp of {name:string; op:Op.t; operand0:Expr.t; operand1:Expr.t; ty:Type.t;}
            | Alloc of {name:string; ty:Type.t} 
            | Store of {operand:Expr.t; name:string; ty:Type.t}
            | Load of {name:string; operand:Expr.t; ty:Type.t}
            | ICmp of {name:string; cond:Cond.t; operand0:Expr.t; operand1:Expr.t; ty:Type.t}
            | Select of {name:string; cond:Expr.t; operand0:Expr.t; operand1:Expr.t; ty:Type.t}
            | ReturnSite of {name:string; ty:Type.t}
            | Call of {name:string; callee:string; args:Expr.t list; ty:Type.t}
            | GetElementPtr of {name:string; ty:Type.t; operand:Expr.t; index:Expr.t list}
            | BitCast of {name:string; operand:Expr.t; ty:Type.t;}
            | Sext of {name:string; operand:Expr.t; ty:Type.t}
            | Zext of {name:string; operand:Expr.t; ty:Type.t}
            | Prune of {cond:string; value:Expr.t}
            | NPrune of {cond:string; value:Expr.t list}
            | Other

let pp ppf (inst:t) =
  match inst with
  | BinaryOp {name; op; operand0; operand1; _;} -> 
      Format.fprintf ppf "%s = %a %a %a" name Op.pp op Expr.pp operand0 Expr.pp operand1
  | Alloc {name; ty} -> 
      Format.fprintf ppf "%s = alloc %a" name Type.pp ty  
  | Store {operand; name; ty} -> 
      Format.fprintf ppf "store %a %a %s" Expr.pp operand Type.pp ty name
  | Load {name; operand; _;} -> 
      Format.fprintf ppf "%s = load %a" name Expr.pp operand
  | ICmp {name; cond; operand0; operand1; _;} -> 
      Format.fprintf ppf "%s = icmp %a %a %a" name Cond.pp cond Expr.pp operand0 Expr.pp operand1
  | Select {name; cond; operand0; operand1; _;} ->  
      Format.fprintf ppf "%s = select %a %a %a" name Expr.pp cond Expr.pp operand0 Expr.pp operand1
  | ReturnSite {name; ty} -> 
      Format.fprintf ppf "%s = %a(call return)" name Type.pp ty
  | GetElementPtr {name; ty; operand; index} -> 
      let _ = Format.fprintf ppf "%s = getelementptr %a %a" name Type.pp ty Expr.pp operand in
      List.iter (fun idx -> Format.fprintf ppf ", %a" Expr.pp idx) index
  | BitCast {name; operand; ty;} ->  
      Format.fprintf ppf "%s = bitcast %a to %a" name Expr.pp operand Type.pp ty
  | Zext {name; operand; ty;} -> 
      Format.fprintf ppf "%s = zext %a to %a" name Expr.pp operand Type.pp ty
  | Sext {name; operand; ty;} -> 
      Format.fprintf ppf "%s = sext %a to %a" name Expr.pp operand Type.pp ty
  | Call {name; _} -> 
      Format.printf "call %s" name
  | Prune {cond; value} -> 
      Format.fprintf ppf "prune %s %a" cond Expr.pp value
  | NPrune {cond; value} -> 
      Format.fprintf ppf "!prune %s %a" cond 
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
      (fun fmt a -> Format.fprintf fmt "%a" Expr.pp a)) value
  | Other -> Format.fprintf ppf "Other"

