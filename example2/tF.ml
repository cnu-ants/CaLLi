open Calli


type memty = AbsMemory.t

let set_constraint  (value:AbsValue.t) (v:AbsValue.t) : AbsValue.t = 
    match value, v with
    | AbsInt v', _ -> AbsInt (v')

    (* | AbsInt v', AbsInt v -> AbsInt (v', S (AbsValue.AbsSet.add v s)) *)
    | _ -> value

let set_constraint_cond (cond:Cond.t)  (value:AbsValue.t) (v:AbsValue.t) : AbsValue.t = 
    match cond with
    | Eq -> AbsValue.app_eq value v
    | Ne -> AbsValue.app_ne value v
    | Slt -> AbsValue.app_slt value v 
    | Sge -> AbsValue.app_sge value v
    | _ -> failwith "set_constraint_cond : not imple"
                    
let abs_eval (e : Expr.t) (mem: AbsMemory.t) =
    match e with
    | ConstInt {value; _} -> AbsValue.alpha (IntLiteral value) ""
    | Name {name;_} -> 
    (try (match Env.find name !Env.env with 
    | a -> AbsMemory.find a mem
    ) with _ -> AbsValue.top)
    | Void _ -> AbsValue.top
    | _ -> AbsValue.top

let nprune s (v:AbsValue.t) mem (meta : Metadata.t) = 
    let a = Env.find s !Env.env in
    let v' =  AbsMemory.find a mem in
    let pruned_v = AbsValue.sub v' v in 
    if AbsValue.(pruned_v <= (AbsValue.bot)) then 
        AbsMemory.bot
    else   
    if mem = AbsMemory.bot then mem else
    match meta with
    | Meta {alias} ->
    (match Metadata.Alias.find_opt s alias with
    | Some (Predicate {cond; operand0; operand1}) -> 
      mem
    | Some (Pointer e) -> 
        let s' = match e with
        | Name {name; _;} -> name 
        | _ -> failwith "error1" in
        let a = Env.find s' !Env.env in
        let a' = AbsMemory.find a mem in
        (match a' with
        | AbsAddr a'' ->
            let mem' = AbsValue.AbsAddr.fold
            (fun a mem ->  
                let v' = AbsMemory.find a mem in 
                let v'' = AbsValue.sub v' v in 
                if AbsValue.(v'' <= AbsValue.bot) then AbsMemory.bot 
                 else AbsMemory.update a v'' mem 
                (* let _ = Pp.printf ~color:Yellow "%a -> %a\n" AbsValue.pp v' AbsValue.pp v'' in
                AbsMemory.update a v'' mem  *)
            ) 
            a'' mem
            in mem'
        | _ -> mem (*failwith "Error"*))
    | _ -> mem
    )
    | Empty -> failwith "Nothing to Prune"

let rec prune s (v:AbsValue.t) mem (meta : Metadata.t) =
    let a = Env.find s !Env.env in
    let v' =  AbsMemory.find a mem in
    let pruned_v = AbsValue.meet v' v in 
    if AbsValue.(pruned_v <= (AbsValue.bot)) then 
        AbsMemory.bot
    else 
    if AbsValue.(v <= (AbsValue.bot)) then mem else
    let t = AbsValue.alpha (IntLiteral (Z.of_int 1)) "" in
    let f = AbsValue.alpha (IntLiteral (Z.of_int 0)) "" in 
    match meta with
    | Meta {alias} ->
    (match Metadata.Alias.find_opt s alias with
    | Some (Predicate {cond; operand0; operand1}) -> 
        (match cond with
        (* NE *)
         | Ne when (v = f) -> 
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            prune name (abs_eval operand1 mem) mem meta
        | ConstInt _, Name {name; _} -> 
            prune name (abs_eval operand0 mem) mem meta
        | _ -> (* let _ = Format.printf "prune not implemeted yet" in *) mem
        )
        | Ne when (v = t) ->
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Ne v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then 
              AbsMemory.bot
            else 
              let mem = AbsMemory.update a pruned_v mem in
              prune name pruned_v mem meta
        | ConstInt _, Name {name; _} -> 
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            prune name (set_constraint v (abs_eval operand0 mem)) mem meta
        | _ -> (* let _ = Format.printf "prune not implemeted yet" in *) mem
        )

        (* EQ *)
        | Eq when (v = t) -> 
        (match operand0, operand1 with
        | ConstInt _, ConstInt _ ->
            if AbsValue.((abs_eval operand0 mem) = (abs_eval operand1 mem)) then
              mem
            else AbsMemory.bot
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond cond v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then 
              AbsMemory.bot
            else 
              let mem = AbsMemory.update a pruned_v mem in
              prune name pruned_v mem meta
        | Name {name=name1; _}, Name {name=name2; _} -> 
            let a = Env.find name1 !Env.env in
            let v =  AbsMemory.find a mem in
            let a = Env.find name1 !Env.env in
            let v =  AbsMemory.find a mem in
            mem
        | _ -> failwith "prune Eq not imp"
        )
        | Eq when (v = f) ->
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Ne v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then 
              AbsMemory.bot
            else 
              let mem = AbsMemory.update a pruned_v mem in
              prune name pruned_v mem meta
        | Name {name=name1; _}, Name {name=name2; _} -> 
            let a = Env.find name1 !Env.env in
            let v =  AbsMemory.find a mem in
            let a = Env.find name1 !Env.env in
            let v =  AbsMemory.find a mem in
            mem
        | _ -> (* let _ = Format.printf "prune not implemeted yet" in *) mem
        )

        (* SLT *)
        | Slt when (v = t)  -> 
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond cond v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot 
            else 
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
            (*let _ = Format.printf "res : %a\n" AbsValue.pp ((abs_eval operand1 mem)) in
            let _ = Format.printf "res : %a\n" AbsValue.pp (set_constraint v (abs_eval operand1 mem)) in
            prune name (set_constraint v (abs_eval operand1 mem)) mem meta *)
        | ConstInt _, Name {name; _} -> 
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            failwith "slt"
        | _ -> (*let _ = Format.printf "Prune Slt" in *)mem
        )
        | Slt when (v = f)  -> 
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Sge v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot else 
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
            (*let _ = Format.printf "res : %a\n" AbsValue.pp ((abs_eval operand1 mem)) in
            let _ = Format.printf "res : %a\n" AbsValue.pp (set_constraint v (abs_eval operand1 mem)) in
            prune name (set_constraint v (abs_eval operand1 mem)) mem meta *)
        | ConstInt _, Name {name; _} -> 
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            failwith "slt"
        | _ -> mem
        )

        | _ -> mem
        )
    | Some (Pointer e) ->
        let a = Env.find s !Env.env in
        let v' =  AbsMemory.find a mem in
        let pruned_v = AbsValue.meet v v' in 
        if AbsValue.(pruned_v <= (AbsValue.bot)) then 
          AbsMemory.bot
        else 
          let mem = AbsMemory.update a pruned_v mem in
        let s' = match e with
        | Name {name; _;} -> name 
        | _ -> failwith "error1" in
        let a = Env.find s' !Env.env in
        let a' = AbsMemory.find a mem in
        (match a' with
        | AbsAddr a'' ->
            let mem' = AbsValue.AbsAddr.fold
            (fun a mem ->  
                let v' = AbsMemory.find a mem in 
                let v'' = AbsValue.meet v v' in 
                if AbsValue.(v'' <= (AbsValue.bot)) then
                     AbsMemory.bot 
                else  AbsMemory.update a v'' mem 
            ) 
            a'' mem
            in mem'
        | _ -> mem (*failwith "Error"*))
    | None -> mem
    )
    | Empty -> failwith "Nothing to Prune"

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
    | Prune {cond; value} -> 
    let a = Env.find cond !Env.env in
    let v = abs_eval value mem in
    let bb = Bbpool.find stmt.bb_name !Bbpool.pool in
    let func = Module.find bb.func_name (Init.llmodule ()) in
    (*let mem' = AbsMemory.update a v mem in
    *)let mem'' = prune cond v mem func.metadata in
    mem''
    | NPrune {cond; value} ->
    let a = Env.find cond !Env.env in
    let v = List.fold_left (fun v' v -> AbsValue.join v' (abs_eval v mem)) AbsValue.bot value in
    let bb = Bbpool.find stmt.bb_name !Bbpool.pool in
    let func = Module.find bb.func_name (Init.llmodule ()) in
    let mem'' = nprune cond v mem func.metadata in
    mem''
      (*
      let v_lst = List.map (fun v -> abs_eval v mem) value in
      let bb = Bbpool.find stmt.bb_name !Bbpool.pool in
      let func = Module.find bb.func_name (Init.llmodule ()) in
      let mem' = List.fold_left (fun mem' v -> 
          nprune cond v mem' func.metadata) mem v_lst in
      mem'
      *)
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
        let call_func : Function.t = Module.find callee (Init.llmodule ()) in
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
    let mem' = abs_interp_term' bb.term mem' in
    mem'
