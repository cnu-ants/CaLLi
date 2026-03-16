open Calli


type memty = AbsMemory.t


                    
let abs_eval (e : Expr.t) (mem: AbsMemory.t) =
    match e with
    | ConstInt {value; _} -> AbsValue.alpha (IntLiteral value) ""
    | Name {name;_} -> 
    (try (match Env.find name !Env.env with 
    | a -> AbsMemory.find a mem
    ) with _ -> AbsValue.top)
    | Void _ -> AbsValue.top
    | _ -> AbsValue.top

let meet_name (name : string) (v : AbsValue.t) mem =
  if AbsValue.(v <= bot) then
    mem
  else
    let a = Env.find name !Env.env in
    let old_v = AbsMemory.find a mem in
    let new_v = AbsValue.meet old_v v in
    if AbsValue.(new_v <= bot) then
      AbsMemory.bot
    else
      AbsMemory.update a new_v mem

let swap_cond (c : Cond.t) : Cond.t =
  match c with
  | Eq  -> Eq
  | Ne  -> Ne
  | Slt -> Sgt
  | Sle -> Sge
  | Sgt -> Slt
  | Sge -> Sle
  | Ult -> Ugt
  | Ule -> Uge
  | Ugt -> Ult
  | Uge -> Ule

let set_constraint_cond (cond:Cond.t)  (value:AbsValue.t) (v:AbsValue.t) : AbsValue.t = 
    match cond with
    | Eq -> AbsValue.app_eq value v
    | Ne -> AbsValue.app_ne value v
    | Slt -> AbsValue.app_slt value v 
    | Sge -> AbsValue.app_sge value v
    | _ -> v

let prune_value (lhs : string) (rhs : Expr.t) mem =
  let v = abs_eval rhs mem in
  meet_name lhs v mem

let prune_predicate (cond : Cond.t) (operand0 : Expr.t) (operand1 : Expr.t) mem =
  match operand0, operand1 with
  | Expr.Name { name; _ }, _ ->
      let a = Env.find name !Env.env in
      let cur_v = AbsMemory.find a mem in
      let rhs_v = abs_eval operand1 mem in
      let pruned_v = set_constraint_cond cond cur_v rhs_v in
      if AbsValue.(pruned_v <= bot) then
        AbsMemory.bot
      else
        AbsMemory.update a pruned_v mem

  | _, Expr.Name { name; _ } ->
      let a = Env.find name !Env.env in
      let cur_v = AbsMemory.find a mem in
      let lhs_v = abs_eval operand0 mem in
      let pruned_v = set_constraint_cond (swap_cond cond) cur_v lhs_v in
      if AbsValue.(pruned_v <= bot) then
        AbsMemory.bot
      else
        AbsMemory.update a pruned_v mem

  | _, _ ->
      let lhs_v = abs_eval operand0 mem in
      let rhs_v = abs_eval operand1 mem in
      let pruned_v = set_constraint_cond cond lhs_v rhs_v in
      if AbsValue.(pruned_v <= bot) then
        AbsMemory.bot
      else
        mem

let prune_pointer (lhs : string) (operand : Expr.t) mem =
  let lhs_addr = Env.find lhs !Env.env in
  let lhs_v = AbsMemory.find lhs_addr mem in
  match operand with
  | Expr.Name { name = src; _ } ->
      let src_addr = Env.find src !Env.env in
      let src_v = AbsMemory.find src_addr mem in
      begin
        match src_v with
        | AbsAddr addrs ->
            AbsValue.AbsAddr.fold
              (fun addr mem_acc ->
                let pointee_v = AbsMemory.find addr mem_acc in
                let pruned_v = AbsValue.meet lhs_v pointee_v in
                if AbsValue.(pruned_v <= bot) then
                  AbsMemory.bot
                else
                  AbsMemory.update addr pruned_v mem_acc)
              addrs
              mem
        | _ ->
            mem
      end
  | _ ->
      mem

let prune (pr : Inst.prune_ty) mem =
  match pr with
  | Inst.Value { lhs; rhs } ->
      prune_value lhs rhs mem

  | Inst.Predicate { cond; operand0; operand1 } ->
      prune_predicate cond operand0 operand1 mem

  | Inst.Pointer { lhs; operand } ->
      prune_pointer lhs operand mem

let abs_interp_stmt (stmt : Stmt.t) (mem: AbsMemory.t) : AbsMemory.t =
    let instr = stmt.inst in
    if mem = AbsMemory.bot then mem else
    match instr with
    | ICmp {name; cond; operand0; operand1; _} ->
    let v1 = abs_eval operand0 mem in
    let v2 = abs_eval operand1 mem in
    let res = AbsValue.compop cond v1 v2 name in
    let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
    let _ = Env.env := Env.add name addr !Env.env in
    AbsMemory.update addr res mem
    | Select {name; operand0; operand1; _;} ->
    let v1 = abs_eval operand0 mem in
    let v2 = abs_eval operand1 mem in
    let res = AbsValue.join v1 v2 in
    let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
    let _ = Env.env := Env.add name addr !Env.env in
    AbsMemory.update addr res mem
    | BinaryOp {name; op; operand0; operand1; _} ->
    let v1 = abs_eval operand0 mem in
    let v2 = abs_eval operand1 mem in
    let res : AbsValue.t = AbsValue.binop op v1 v2 name in
    let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
    let _ = Env.env := Env.add name addr !Env.env in
    AbsMemory.update addr res mem
    | Trunc {name; operand; _}
    | Sext {name; operand; _}
    | Zext {name; operand; _} -> 
      let v = abs_eval operand mem in
      let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
      let _ = Env.env := Env.add name addr !Env.env in
      AbsMemory.update addr v mem
    | Alloc {name; _} -> 
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let a = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 1) in
        let addr' = AbsValue.alpha (AddrLiteral a) name in
        let mem' = AbsMemory.update addr addr' mem in
        let _ = Env.env := Env.add name addr !Env.env in
        mem'
    | Store {operand; name; _} -> 
    let v = abs_eval operand mem in
    let a = Env.find name !Env.env in
    let a' = AbsMemory.find a mem in
    (match a' with
    | AbsAddr a'' ->
        let mem' = AbsValue.AbsAddr.fold
        (fun a mem ->  AbsMemory.update a v mem ) a'' mem
        in mem'
    | _ ->  mem)
    | IntToPtr {name; _} -> 
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let a = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 1) in
        let addr' = AbsValue.alpha (AddrLiteral a) name in
        let mem' = AbsMemory.update addr addr' mem in
        let _ = Env.env := Env.add name addr !Env.env in
        mem'
    | Load {name; operand; _} -> 
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let res = abs_eval operand mem in
        let res' = 
        (match res with
        | AbsAddr a -> 
            AbsValue.AbsAddr.fold
            (fun a' v -> AbsValue.join v (AbsMemory.find a' mem)) a AbsValue.bot
        | AbsTop -> AbsValue.top
        | AbsBot -> AbsValue.bot 
        | AbsInt _ -> AbsValue.top (*
          let _ = Format.printf "load error inst : %a\n@." Inst.pp stmt.inst in
          let _ = Format.printf "%a\n" AbsMemory.pp mem in
          let _ = Format.printf "%a\n" Env.pp !Env.env in
          let _ = Format.printf "%a\n" AbsValue.pp res in
          failwith "load error" *)
        ) in
        let mem' = AbsMemory.update addr res' mem in
        let _ = Env.env := Env.add name addr !Env.env in
        mem'
    | ReturnSite {name; ty} ->
    let res = abs_eval (Expr.Name {ty=ty; name="ret"}) mem in

    let addr = stmt.bb_name^"return" in
    let _ = Env.env := Env.add name addr !Env.env in
    let mem' = AbsMemory.update addr res mem in
    mem'      
    | Prune pr ->
      prune pr mem
    | _ -> mem



let abs_interp_term' (term : Term.t) (mem : AbsMemory.t) = 
    if mem = AbsMemory.bot then mem else
    match term with
    | Br _ -> mem
    | CondBr _ -> mem
    | Ret {ret; bb_name} -> 
    let res = abs_eval ret mem in
    let addr = bb_name^(string_of_int (-1))^(string_of_int 0) in
    let _ = Env.env := Env.add "ret" addr !Env.env in
    let mem' = AbsMemory.update addr res mem in
    mem'
    | Exit _ -> mem
    | CallSite {callee; args; bb_name;_} ->
        let call_func  = Module.find_opt callee (Init.llmodule ()) in
        (match call_func with
        |Some call_func ->
        if (List.length call_func.params) <> (List.length args) then mem else
        let mem', _ =
          List.fold_left
          (fun (mem, index) arg ->
            let param = List.nth call_func.params index in
            let res = abs_eval arg mem in
            let addr = bb_name^"param"^(string_of_int index) in
            let name = (Expr.typed_var_of_expr param).name in
            let _ = Env.env := Env.add name addr !Env.env in
            let mem' = AbsMemory.update addr res mem in
            (mem', index+1)
          )
          (mem, 0)
          args
        in
        mem'
        | None -> mem)
    | Switch _ -> mem
    | _ -> mem

let abs_interp_global (v : Global.t) mem = 
    let res = abs_eval v.value mem in
    let addr = "global"^v.name^(string_of_int 0) in
    let a = "global"^v.name^(string_of_int 1) in
    let addr' = AbsValue.alpha (AddrLiteral a) v.name in      
    let mem' = AbsMemory.update addr addr' mem in
    let mem' = AbsMemory.update a res mem' in
    let _ = Env.env := Env.add v.name addr !Env.env in
    mem'


let transfer (bb : Basicblock.t) (mem : AbsMemory.t)  =
    let _ = Format.printf "TF : %s@." bb.bb_name in 
    let mem' = List.fold_left
    (fun mem stmt ->
        let mem'' = abs_interp_stmt stmt mem in
        mem''
    )
    mem bb.stmts 
    in
    let mem' = 
      match bb.term with
      | Some term ->  abs_interp_term' term mem' 
      | None -> mem'
    in
    mem'
