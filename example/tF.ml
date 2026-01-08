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
      | "" -> if name = "Func_main(i32%arg_esp)i32%arg_esp" then AbsValue.alpha (IntLiteral (String_addr.id_of_string name)) "" else AbsValue.top
      | a -> AbsMemory.find a mem
      ) with _ -> AbsValue.alpha (IntLiteral (String_addr.id_of_string name)) "" )
    | Void _ -> AbsValue.top
    | _ -> AbsValue.top


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
    | IntToPtr {name; operand; _} -> 
        let addr = stmt.bb_name^(string_of_int stmt.index)^(string_of_int 0) in
        let a = abs_eval operand mem in
        let a_str = Format.asprintf "%a" AbsValue.pp a in
        let addr' = AbsValue.alpha (AddrLiteral a_str) "" in
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
    | CallSite {callee; args; bb_name;_} -> mem
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
    let mem' = List.fold_left
    (fun mem stmt ->
        let mem'' = abs_interp_stmt stmt mem in
        mem''
    )
    mem bb.stmts 
    in
    let mem' = abs_interp_term' bb.term mem' in
    (*let _ = Format.printf "%s@." bb.bb_name in
    let _ = Format.printf "%a@." AbsMemory.pp mem' in
    *)mem'
