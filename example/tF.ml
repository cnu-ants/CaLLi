open Calli
module AbsValue = AbsValue

type memty = AbsMemory.t 

let tmp_addr = ref 100000

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
    | Sle -> AbsValue.app_sle value v 
    | Sge -> AbsValue.app_sge value v
    | Sgt -> AbsValue.app_sgt value v
    | _ -> failwith "set_constraint_cond : not imple"

let addr_of_name (name : string) : string =
  name ^ "#addr"

let obj_addr_of_name (name : string) : string =
  name ^ "#obj"

let ret_addr : string =
  "#ret"

                    
let abs_eval (e : Expr.t) (mem: AbsMemory.t) =
    match e with
    | ConstInt {value; _} -> AbsValue.alpha (IntLiteral value) ""
    | Name {name;_} -> 
      (try (match Env.find name !Env.env with 
      | "" -> if name = "Func_main(i32%arg_esp,i8**%argv)i32%arg_esp" then AbsValue.alpha (IntLiteral (Z.of_int !tmp_addr)) "" else AbsValue.top
      | a -> AbsMemory.find a mem
      ) with _ -> AbsValue.alpha (IntLiteral (String_addr.id_of_string name)) "" )
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
        | AbsAddr (AddrSet a'') ->
            let mem' = AbsValue.AbsAddr.fold
            (fun a mem ->  
                let v' = AbsMemory.find a mem in 
                let v'' = AbsValue.sub v' v in 
                if AbsValue.(v'' <= AbsValue.bot) then AbsMemory.bot 
                 else AbsMemory.update a v'' mem 
                (* let _ = Pp.printf ~color:Yellow "%a -> %a\n" AbsValue.pp v' AbsValue.pp v'' in
                AbsMemory.update a v'' mem  *)
            ) 
            (AddrSet a'') mem
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
        (*let _ = Format.printf "PRUNE %a %a %a@." Cond.pp cond Expr.pp operand0 Expr.pp operand1 in
        let _ = Format.printf "CURRENT %a %a@." Expr.pp operand0 AbsValue.pp (abs_eval operand0 mem) in *)
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
              let _ = Format.printf "******EQ BOTTOM: %s@.*******" a in
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

        (* SGT *)
        | Sgt when (v = t)  -> (* true *)
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in (* env에서 name의 absAddr을 찾음 *)
            let v =  AbsMemory.find a mem in (* absmem에서 absAddr의 absVal을 찾음 *)
            let pruned_v = set_constraint_cond cond v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot 
            else 
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
            (*prune name (set_constraint v (abs_eval operand1 mem)) mem meta*) 
        | ConstInt _, Name {name; _} -> 
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            failwith "sgt" 
        | Name {name=name0; _}, Name {name=name1; _} ->
          let a0 = Env.find name0 !Env.env in
          let v0 = AbsMemory.find a0 mem in
          let a1 = Env.find name1 !Env.env in
          let v1 = AbsMemory.find a1 mem in
          (* let _ = Format.printf "before prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp v0 name1 AbsValue.pp v1 in *)
          
          let _ = Format.printf "Sgt prune: %s=%a, %s=%a\n"
          name0 AbsValue.pp v0
          name1 AbsValue.pp v1 in
          
          (match v0, v1 with
          | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1}),
            AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2}) ->
              (* x = [max(min1, min2+1), max1] *)
              let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [min1; AbsInterval.Elt.(min2 + AbsInterval.Elt.one)]) max1) in
              (* y = [min2, min(max1-1, max2)] *)
              let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval min2 (AbsInterval.Elt.min_elt [AbsInterval.Elt.(max1 - AbsInterval.Elt.one); max2])) in
              (* let _ = Format.printf "after prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp pruned_v0 name1 AbsValue.pp pruned_v1 in *)
              
              let _ = Format.printf "pruned_v0=%a, pruned_v1=%a\n"
              AbsValue.pp pruned_v0 AbsValue.pp pruned_v1 in
              
              if AbsValue.(pruned_v0 <= AbsValue.bot) || AbsValue.(pruned_v1 <= AbsValue.bot) then
                AbsMemory.bot
              else
                let mem = AbsMemory.update a0 pruned_v0 mem in
                let mem = AbsMemory.update a1 pruned_v1 mem in
                let mem = prune name0 pruned_v0 mem meta in
                let mem = prune name1 pruned_v1 mem meta in
                mem
          | _ -> mem)
        | _ -> (*let _ = Format.printf "Prune Slt" in*) mem
        )
        | Sgt when (v = f)  -> (* false *)
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Sle v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot else 
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
        | ConstInt _, Name {name; _} -> 
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            failwith "slt"
        | Name {name=name0; _}, Name {name=name1; _} ->
          let a0 = Env.find name0 !Env.env in
          let v0 = AbsMemory.find a0 mem in
          let a1 = Env.find name1 !Env.env in
          let v1 = AbsMemory.find a1 mem in
          let _ = Format.printf "before prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp v0 name1 AbsValue.pp v1 in
          (match v0, v1 with
          | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1}),
            AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2}) ->
              (* x = [min1, min(max1, max2)] *)
              let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval min1 (AbsInterval.Elt.min_elt [max1; max2])) in
              (* y = [max(min1, min2), max2] *)
              let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [min1; min2]) max2) in
              let _ = Format.printf "after prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp pruned_v0 name1 AbsValue.pp pruned_v1 in
              if AbsValue.(pruned_v0 <= AbsValue.bot) || AbsValue.(pruned_v1 <= AbsValue.bot) then
                AbsMemory.bot
              else
                let mem = AbsMemory.update a0 pruned_v0 mem in
                let mem = AbsMemory.update a1 pruned_v1 mem in
                let mem = prune name0 pruned_v0 mem meta in
                let mem = prune name1 pruned_v1 mem meta in
                mem
          | _ -> mem)
        | _ -> mem
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
        | Name {name=name0; _}, Name {name=name1; _} -> 
           let a0 = Env.find name0 !Env.env in 
           let v0 = AbsMemory.find a0 mem in 
           let a1 = Env.find name1 !Env.env in 
           let v1 = AbsMemory.find a1 mem in 
           (match v0, v1 with
           | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1}),
             AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2}) ->
               let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval min1 (AbsInterval.Elt.min_elt [max1; AbsInterval.Elt.(max2 - AbsInterval.Elt.one)])) in
               let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [AbsInterval.Elt.(min1 + AbsInterval.Elt.one); min2]) max2) in
               if AbsValue.(pruned_v0 <= AbsValue.bot) || AbsValue.(pruned_v1 <= AbsValue.bot) then
                 AbsMemory.bot
               else
                 let mem = AbsMemory.update a0 pruned_v0 mem in
                 let mem = AbsMemory.update a1 pruned_v1 mem in
                 let mem = prune name0 pruned_v0 mem meta in
                 let mem = prune name1 pruned_v1 mem meta in
                 mem
           | _ -> mem)
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
          let s' = (match e with
            | Name {name; _;} -> name 
            | _ -> failwith "error1") in
          let a = Env.find s' !Env.env in
          let a' = AbsMemory.find a mem in
          (match a' with
          | AbsAddr (AddrSet a'') ->
            let mem' = AbsValue.AbsAddr.fold
            (fun a mem ->  
                let v' = AbsMemory.find a mem in 
                let v'' = AbsValue.meet pruned_v v' in 
                if AbsValue.(v'' <= (AbsValue.bot)) then
                     AbsMemory.bot 
                else  AbsMemory.update a v'' mem 
            ) 
            (AddrSet a'') mem
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
      let addr = Env.find name !Env.env in
      AbsMemory.update addr res mem

    | Select {name; operand0; operand1; _} ->
      let v1 = abs_eval operand0 mem in
      let v2 = abs_eval operand1 mem in
      let res = AbsValue.join v1 v2 in
      let addr = Env.find name !Env.env in
      AbsMemory.update addr res mem

    | BinaryOp {name; op; operand0; operand1; _} ->
      let v1 = abs_eval operand0 mem in
      let v2 = abs_eval operand1 mem in
      let res : AbsValue.t = AbsValue.binop op v1 v2 name in
      let addr = Env.find name !Env.env in
      AbsMemory.update addr res mem

    | Trunc {name; operand; _}
    | Sext {name; operand; _}
    | Zext {name; operand; _} ->
      let v = abs_eval operand mem in
      let addr = Env.find name !Env.env in
      AbsMemory.update addr v mem

    | Alloc {name; _} ->
      let addr = Env.find name !Env.env in
      let obj_addr = obj_addr_of_name name in
      let ptr_v = AbsValue.alpha (AddrLiteral obj_addr) name in
      let mem' = AbsMemory.update obj_addr AbsValue.bot mem in
      AbsMemory.update addr ptr_v mem'

    | Store {operand; name; _} ->
      let v = abs_eval operand mem in
      let a = Env.find name !Env.env in
      let a' = AbsMemory.find a mem in
      (match a' with
      | AbsAddr a'' ->
          AbsValue.AbsAddr.fold
            (fun a mem -> AbsMemory.update a v mem)
            a'' mem
      | _ -> mem)

    | IntToPtr {name; operand; _} ->
      let addr = Env.find name !Env.env in
      let a = abs_eval operand mem in
      let mem' =   
          (match a with
          | AbsInt i ->
              let addr' = AbsValue.AbsAddr.AddrSet
                (AbsValue.AbsInt.fold
                (fun i addrset ->
                  let s = AbsValue.AbsInt.to_string i in
                  AbsValue.AbsAddr.S.add s addrset)
                i AbsValue.AbsAddr.S.empty)
              in 
              AbsMemory.update addr (AbsAddr addr') mem
          | AbsBot -> 
              AbsMemory.update addr AbsValue.bot mem
          | _ -> 
              AbsMemory.update addr AbsValue.top mem

              (*let _ = Format.printf "%a@.%a@." AbsValue.pp a AbsMemory.pp mem in
              let _ = Format.printf "--\n %a@.%a@.--\n" AbsValue.pp a AbsMemory.pp mem in
              let _ = Format.printf "==ENV==\n %a@." Env.pp !Env.env in
              let _ = Format.printf "&& inttoptr inst &&@." in
              let _ = Format.printf "%a@." Inst.pp instr in
              failwith "InttoPtr err")*))
      in
      mem'

    | PHI {name; incoming; _} ->
      let result =
        List.fold_left
          (fun acc (value, _) ->
            let v = abs_eval value mem in
            AbsValue.join acc v)
          AbsValue.bot incoming
      in
      let addr = Env.find name !Env.env in
      let _ = Format.printf "PHI %s addr=%s result=%a\n" 
      name addr AbsValue.pp result in
      AbsMemory.update addr result mem

    | Load {name; operand; _} ->
      let addr = Env.find name !Env.env in
      let res = abs_eval operand mem in
      let res' =
        match res with
        | AbsAddr a ->
            AbsValue.AbsAddr.fold
              (fun a' v -> AbsValue.join v (AbsMemory.find a' mem))
              a AbsValue.bot
        | AbsTop -> AbsValue.top
        | AbsBot -> AbsValue.bot
        | AbsInt _ -> AbsValue.top
      in
      AbsMemory.update addr res' mem

    | Prune {cond; value} ->
      let _a = Env.find cond !Env.env in
      let v = abs_eval value mem in
      let bb = Bbpool.find stmt.bb_name !Bbpool.pool in
      let func = Module.find bb.func_name (Init.llmodule ()) in
      let mem'' = prune cond v mem func.metadata in
      mem''

    | NPrune {cond; value} ->
      let _a = Env.find cond !Env.env in
      let v =
        List.fold_left
          (fun v' v -> AbsValue.join v' (abs_eval v mem))
          AbsValue.bot value
      in
      let bb = Bbpool.find stmt.bb_name !Bbpool.pool in
      let func = Module.find bb.func_name (Init.llmodule ()) in
      let mem'' = nprune cond v mem func.metadata in
      mem''

    | ReturnSite {name; ty} ->
      let res = abs_eval (Expr.Name {ty=ty; name="ret"}) mem in
      let addr = Env.find name !Env.env in
      AbsMemory.update addr res mem

    | PtrToInt {name; operand; _} ->
      let v = abs_eval operand mem in
      let addr = Env.find name !Env.env in
      let res =
        match v with
        | AbsAddr addrs ->
            let ints =
              AbsValue.AbsAddr.fold
                (fun a acc ->
                  try
                    let z = Z.of_string a in
                    AbsValue.join acc (AbsValue.alpha (IntLiteral z) name)
                  with _ -> AbsValue.top)
                addrs AbsValue.bot
            in
            ints
        | AbsBot -> AbsValue.bot
        | _ -> AbsValue.top
      in
      AbsMemory.update addr res mem

    | _ -> mem

let abs_interp_term' (term : Term.t) (mem : AbsMemory.t) =
    if mem = AbsMemory.bot then mem else
    match term with
    | Br _ -> mem
    | CondBr _ -> mem
    | Ret {ret; _} ->
      let res = abs_eval ret mem in
      let addr = Env.find "ret" !Env.env in
      AbsMemory.update addr res mem
    | Exit _ -> mem
    | CallSite _ -> mem
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



let seed_entry_defs_bot (f : Function.t) (mem : AbsMemory.t) : AbsMemory.t =
  let mem =
    List.fold_left
      (fun mem v ->
        match v with
        | Expr.Name {name; _} ->
            let addr = addr_of_name name in
            let _ = Env.env := Env.add name addr !Env.env in
            AbsMemory.update addr AbsValue.bot mem
        | _ -> mem)
      mem
      f.vars
  in
  let _ = Env.env := Env.add "#ret" ret_addr !Env.env in
  AbsMemory.update ret_addr AbsValue.bot mem

let transfer (bb : Basicblock.t) (mem : AbsMemory.t)  =
    let mem' = List.fold_left
    (fun mem stmt ->
        let mem'' = abs_interp_stmt stmt mem in
        mem''
    )
    mem bb.stmts 
    in
    let mem' = abs_interp_term' bb.term mem' in
    (* let _ = Format.printf "AFTER %a@." AbsMemory.pp mem' in
    let _ = Format.printf "%s@." bb.bb_name in
    let _ = Format.printf "%a@." AbsMemory.pp mem' in *)
    mem'
