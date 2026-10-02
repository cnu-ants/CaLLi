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
(* let _ = Format.printf "  [scc] cond=%a\n" Cond.pp cond in *)
    match cond with
    | Eq -> AbsValue.app_eq value v
    | Ne -> AbsValue.app_ne value v
    | Slt -> AbsValue.app_slt value v 
    | Sle -> AbsValue.app_sle value v 
    | Sge -> AbsValue.app_sge value v
    | Sgt -> AbsValue.app_sgt value v
    | Ugt -> AbsValue.app_ugt value v 
    | Uge -> AbsValue.app_uge value v
    | Ult -> AbsValue.app_ult value v
    | Ule -> AbsValue.app_ule value v
    (* | _ -> failwith "set_constraint_cond : not imple" *)

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
      | "" -> if ((name = "Func_main(i32%arg_esp,i8**%argv)i32%arg_esp")) then AbsValue.alpha (IntLiteral (Z.of_int !tmp_addr)) "" else AbsValue.top
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
        (* let _ =Format.printf "alias of %s: Predicate %a %a %a\n" s Cond.pp cond Expr.pp operand0 Expr.pp operand1 in *)
        (* let _ = Format.printf "PRUNE %a %a %a@." Cond.pp cond Expr.pp operand0 Expr.pp operand1 in *)
        (* let _ = Format.printf "CURRENT %a %a@." Expr.pp operand0 AbsValue.pp (abs_eval operand0 mem) in *)
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
              (* let _ = Format.printf "******EQ BOTTOM: %s@.*******" a in *)
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
        | _ -> let _ = Format.printf "prune not implemeted yet" in mem
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
          (* let _ = Format.printf "prune sgt true\nbefore prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp v0 name1 AbsValue.pp v1 in *)
          (match v0, v1 with
          | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1}),
            AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2}) ->
              (* x = [max(min1, min2+1), max1] *)
              let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [min1; AbsInterval.Elt.(min2 + AbsInterval.Elt.one)]) max1) in
              (* y = [min2, min(max1-1, max2)] *)
              let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval min2 (AbsInterval.Elt.min_elt [AbsInterval.Elt.(max1 - AbsInterval.Elt.one); max2])) in
              (* let _ = Format.printf "after prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp pruned_v0 name1 AbsValue.pp pruned_v1 in *)
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
          (* let _ = Format.printf "prune sgt false\nbefore prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp v0 name1 AbsValue.pp v1 in *)
          (match v0, v1 with
          | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1}),
            AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2}) ->
              (* x = [min1, min(max1, max2)] *)
              let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval min1 (AbsInterval.Elt.min_elt [max1; max2])) in
              (* y = [max(min1, min2), max2] *)
              let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [min1; min2]) max2) in
              (* let _ = Format.printf "after prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp pruned_v0 name1 AbsValue.pp pruned_v1 in *)
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

        (* UGT *)
        | Ugt when (v = t)  -> (* true: x >u k *)
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
        | ConstInt _, Name {name; _} -> 
            (* k >u x  ==  x <u k *)
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Ult v (abs_eval operand0 mem) in
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot
            else
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
        | Name {name=name0; _}, Name {name=name1; _} ->
          let a0 = Env.find name0 !Env.env in
          let v0 = AbsMemory.find a0 mem in
          let a1 = Env.find name1 !Env.env in
          let v1 = AbsMemory.find a1 mem in
          (* let _ = Format.printf "prune ugt true\nbefore prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp v0 name1 AbsValue.pp v1 in *)
          (match v0, v1 with
          | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1} as n0),
            AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2} as n1')
            when AbsInterval.all_nonneg n0 && AbsInterval.all_nonneg n1' ->
              (* 둘 다 비음수 -> unsigned 순서 = signed 순서, Sgt와 동일한 계산 *)
              let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [min1; AbsInterval.Elt.(min2 + AbsInterval.Elt.one)]) max1) in
              let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval min2 (AbsInterval.Elt.min_elt [AbsInterval.Elt.(max1 - AbsInterval.Elt.one); max2])) in
              (* let _ = Format.printf "after prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp pruned_v0 name1 AbsValue.pp pruned_v1 in *)
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
        | Ugt when (v = f)  -> (* false: x <=u k *)
        (match operand0, operand1 with
        | Name {name; _}, ConstInt _ -> 
            let a = Env.find name !Env.env in
            let v =  AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Ule v (abs_eval operand1 mem) in 
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot else 
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
        | ConstInt _, Name {name; _} -> 
            (* not (k >u x)  ==  x >=u k *)
            let a = Env.find name !Env.env in
            let v = AbsMemory.find a mem in
            let pruned_v = set_constraint_cond Cond.Uge v (abs_eval operand0 mem) in
            if AbsValue.(pruned_v <= (AbsValue.bot)) then
              AbsMemory.bot
            else
            let mem = AbsMemory.update a pruned_v mem in
            prune name pruned_v mem meta
        | Name {name=name0; _}, Name {name=name1; _} ->
          let a0 = Env.find name0 !Env.env in
          let v0 = AbsMemory.find a0 mem in
          let a1 = Env.find name1 !Env.env in
          let v1 = AbsMemory.find a1 mem in
          (* let _ = Format.printf "prune ugt false\nbefore prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp v0 name1 AbsValue.pp v1 in *)
          (match v0, v1 with
          | AbsValue.AbsInt (AbsInterval.IntInterval {min=min1; max=max1} as n0),
            AbsValue.AbsInt (AbsInterval.IntInterval {min=min2; max=max2} as n1')
            when AbsInterval.all_nonneg n0 && AbsInterval.all_nonneg n1' ->
              let pruned_v0 = AbsValue.AbsInt (AbsInterval.mk_interval min1 (AbsInterval.Elt.min_elt [max1; max2])) in
              let pruned_v1 = AbsValue.AbsInt (AbsInterval.mk_interval (AbsInterval.Elt.max_elt [min1; min2]) max2) in
              (* let _ = Format.printf "after prune: %s -> %a, %s -> %a\n" name0 AbsValue.pp pruned_v0 name1 AbsValue.pp pruned_v1 in *)
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
        (* let _ = Format.printf "alias of %s: Pointer %a\n" s Expr.pp e in *)
        let a = Env.find s !Env.env in (* abstract addr 가져옴 *)
        let v' =  AbsMemory.find a mem in (* abstract value 가져옴 *)
        let pruned_v = AbsValue.meet v v' in (* prune하려는 값(v)과 현재값(v')을 meet *)
        if AbsValue.(pruned_v <= (AbsValue.bot)) then 
          AbsMemory.bot
        else 
          let mem = AbsMemory.update a pruned_v mem in
          let s' = 
            (match e with
            | Name {name; _;} -> name 
            | _ -> failwith "error1") 
          in
          let a = Env.find s' !Env.env in
          let a' = AbsMemory.find a mem in
          (match a' with
          | AbsAddr (AddrSet a'') ->
            let mem' = AbsValue.AbsAddr.fold
            (fun a mem ->  
                let v' = AbsMemory.find a mem in 
                let v'' = AbsValue.meet pruned_v v' in 
                (* let _ = Format.printf "Pointer fold: addr=%s v'=%a pruned_v=%a meet=%a\n"
    a AbsValue.pp v' AbsValue.pp pruned_v AbsValue.pp v'' in *)
                if AbsValue.(v'' <= (AbsValue.bot)) then
                  (* let _ = Format.printf "→ bot! addr=%s v'=%a pruned_v=%a\n" 
                  a AbsValue.pp v' AbsValue.pp pruned_v in *)
                     AbsMemory.bot 
                else  AbsMemory.update a v'' mem 
                (* AbsMemory.update a pruned_v mem *)
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
      (* let _ = Format.printf "[inttoptr] %s <- %a  (bb=%s)@."
        name AbsValue.pp a stmt.bb_name in *)
      let mem' =
          (match a with
          (* 추가: 무한 구간이면 fold를 부르지 않고 no-op *)
          | AbsInt (AbsInterval.IntInterval {min; max})
            when not (AbsValue.AbsInt.is_finite_elt min
                      && AbsValue.AbsInt.is_finite_elt max) ->
              (* let _ = Format.printf
                "[inttoptr] skip: infinite interval %s <- %a (bb=%s)@."
                name AbsValue.pp a stmt.bb_name in *)
              (* AbsMemory.update addr AbsValue.top mem *)
            mem
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
            )
      in
      mem'

    (* | IntToPtr {name; operand; _} ->
      let addr = Env.find name !Env.env in
      let a = abs_eval operand mem in
      let _ = Format.printf "[inttoptr] %s <- %a  (bb=%s)@."
      name AbsValue.pp a stmt.bb_name in
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
              (* let _ = Format.printf "%a@.%a@." AbsValue.pp a AbsMemory.pp mem in
              let _ = Format.printf "--\n %a@.%a@.--\n" AbsValue.pp a AbsMemory.pp mem in
              let _ = Format.printf "==ENV==\n %a@." Env.pp !Env.env in *)
              
            )
      in
      mem' *)

    | PHI {name; incoming; _} ->
      let result =
        List.fold_left
          (fun acc (value, _) ->
            let v = abs_eval value mem in
            AbsValue.join acc v)
          AbsValue.bot incoming
      in
      let addr = Env.find name !Env.env in
      AbsMemory.update addr result mem

    (* | Load {name; operand; _} ->
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
      AbsMemory.update addr res' mem *)
      | Load {name; operand; _} ->
        let addr = Env.find name !Env.env in
        let res = abs_eval operand mem in
        (* 디버그 1: operand의 추상값 확인 *)
        (* let _ = Format.printf "[DEBUG Load] name=%s, operand res=%a\n" name AbsValue.pp res in *)
        let res' =
          match res with
          | AbsAddr a ->
              let result = AbsValue.AbsAddr.fold
                (fun a' v -> 
                  let loaded = AbsMemory.find a' mem in
                  (* 디버그 2: 각 주소에서 읽은 값 확인 *)
                  (* let _ = Format.printf "[DEBUG Load] addr=%s, loaded=%a, acc=%a\n" 
                            a' AbsValue.pp loaded AbsValue.pp v in *)
                  AbsValue.join v loaded)
                a AbsValue.bot
              in
              (* 디버그 3: fold 최종 결과 확인 *)
              (* let _ = Format.printf "[DEBUG Load] fold result=%a\n" AbsValue.pp result in *)
              result
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
      let res = abs_eval (Expr.Name {ty=ty; name=ret_addr}) mem in
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

(* s 안에 sub가 부분 문자열로 들어있는지 *)
let contains_substring s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec aux i =
    if i + lsub > ls then false
    else if String.sub s i lsub = sub then true
    else aux (i + 1)
  in
  aux 0

(* memset/memcpy처럼 바이트 단위 일괄 접근으로 생긴 범위를 별도로 기록.
   폭 정보의 최하위 우선순위 fallback으로만 쓰인다. *)
  let bulk_ranges : (int * int) list ref = ref []

  let add_bulk_range s e =
    bulk_ranges := (s, e) :: !bulk_ranges

(* 덩어리(범위) 저장소. (start_offset, end_offset) 리스트.
   memset/memcpy/loop 탐지가 공통으로 여기에 기록한다. *)
  let chunks : (int * int) list ref = ref []

  (* 중복 없이 덩어리 하나를 추가 *)
  let add_chunk (s : int) (e : int) : unit =
    if not (List.mem (s, e) !chunks) then
      chunks := (s, e) :: !chunks

(* (* memset(dst, c, n): dst부터 n바이트를 c로 채운다.
   dst는 단일 주소일 수도, 객체 전체를 나타내는 주소 집합
   (예: malloc이 만든 cell_set)일 수도 있다. 어느 쪽이든 그 집합의
   최소 원소를 "객체의 시작 주소"로 보고, 거기서부터 1바이트씩 n개의
   주소 전부를 c로 strong update한다 (word 단위로 건너뛰지 않음 —
   memset은 byte-level 연산이므로). *)
let model_memset (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  (* args = [dst; c; n; isvolatile] *)
  let dst_e, c_e, n_e =
    match args with
    | dst :: c :: n :: _ -> (dst, c, n)
    | _ -> failwith "model_memset: unexpected number of args"
  in
  let dst = abs_eval dst_e mem in
  let c   = abs_eval c_e mem in
  let n   = abs_eval n_e mem in

  (* 가드 1: dst가 AbsAddr인가. singleton을 요구하지 않는다 — malloc처럼
      다중 원소 AddrSet(cell_set)이어도, 그 집합의 최소 원소가 곧
      객체의 시작 주소이므로 min_elt로 base를 뽑으면 된다. *)
  let base_str =
    match dst with
    | AbsValue.AbsAddr a ->
        (try AbsValue.AbsAddr.min_elt a
          with _ -> failwith "model_memset: dst AddrSet is empty")
    | _ -> 
      failwith "model_memset: dst is not an address"
  in

  (* 가드 2: base가 숫자 주소인가 *)
  let base_int =
    try int_of_string base_str
    with _ -> failwith "model_memset: dst address is not numeric"
  in

  (* 가드 3: n이 상수인가 *)
  let n_int =
    match n with
    | AbsValue.AbsInt (AbsInterval.IntInterval {min; max})
      when AbsInterval.Elt.(min == max) ->
        (match min with
          | AbsInterval.I z -> Z.to_int z
          | _ -> failwith "model_memset: n is infinite")
    | _ -> failwith "model_memset: n is not a constant"
  in

  let start_offset = base_int - !tmp_addr in
  let end_offset = start_offset + n_int - 1 in
  let _ = add_chunk start_offset end_offset in
  let _ = add_bulk_range start_offset end_offset in

  (* 효과: base부터 1씩, n_int개의 byte 주소를 c로 strong update *)
  let rec fill mem i =
    if i >= n_int then mem
    else
      let addr = string_of_int (base_int + i) in
      let mem = AbsMemory.update addr c mem in
      fill mem (i + 1)
  in
  fill mem 0 *)
    
(* (* memcpy(dst, src, n): dst부터 n바이트를 src에서 복사.
   지금은 stack shape 목적이라 값 복사는 하지 않고,
   덩어리 범위만 기록한다. *)
let model_memcpy (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  (* args = [dst; src; n; isvolatile] *)
  let dst_e, n_e =
    match args with
    | dst :: _src :: n :: _ -> (dst, n)
    | _ -> failwith "model_memcpy: unexpected number of args"
  in
  let dst = abs_eval dst_e mem in
  let n   = abs_eval n_e mem in

  (* dst가 단일 주소인가 *)
  let _ = Format.printf "mem \n %a \n" AbsMemory.pp mem in 
  let base_int =
    match dst with
    | AbsValue.AbsAddr a when AbsValue.AbsAddr.is_singleton a ->
        (try int_of_string (AbsValue.AbsAddr.min_elt a)
          with _ -> failwith "model_memcpy: dst address is not numeric")
    | _ -> failwith "model_memcpy: dst is not a single address"
  in

  (* n이 상수인가 *)
  let n_int =
    match n with
    | AbsValue.AbsInt (AbsInterval.IntInterval {min; max})
      when AbsInterval.Elt.(min == max) ->
        (match min with
          | AbsInterval.I z -> Z.to_int z
          | _ -> failwith "model_memcpy: n is infinite")
    | _ -> failwith "model_memcpy: n is not a constant"
  in

  (* 덩어리 기록 *)
  let start_offset = base_int - !tmp_addr in
  let end_offset = start_offset + n_int - 1 in
  let _ = add_chunk start_offset end_offset in
  let _ = add_bulk_range start_offset end_offset in

  (* 메모리는 건드리지 않고 그대로 반환 *)
  mem *)


let model_memset (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  match args with
  | dst_e :: c_e :: n_e :: _ ->
      let dst = abs_eval dst_e mem in
      let n = abs_eval n_e mem in

      let base_opt =
        match dst with
        | AbsValue.AbsAddr a ->
            (match AbsValue.AbsAddr.extract_value_string a with
             | Some s -> int_of_string_opt s
             | None -> None)
        | _ -> None
      in
      let size_opt =
        match n with
        | AbsValue.AbsInt i ->
            (match AbsInterval.singleton_value i with
             | Some z when Z.fits_int z -> Some (Z.to_int z)
             | _ -> None)
        | _ -> None
      in

      (match base_opt, size_opt with
       | Some base_int, Some n_int when n_int > 0 ->
           let c = abs_eval c_e mem in
           let start_offset = base_int - !tmp_addr in
           let end_offset = start_offset + n_int - 1 in
           add_chunk start_offset end_offset;
           add_bulk_range start_offset end_offset;

           let rec fill mem i =
             if i >= n_int then mem
             else
               let addr = string_of_int (base_int + i) in
               fill (AbsMemory.update addr c mem) (i + 1)
           in
           fill mem 0
       | _ -> mem)
  | _ -> mem

  let model_memcpy (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  match args with
  | dst_e :: _src_e :: n_e :: _ ->
      let dst = abs_eval dst_e mem in
      let n = abs_eval n_e mem in
      (match dst, n with
       | AbsValue.AbsAddr a, AbsValue.AbsInt size
         when AbsValue.AbsAddr.is_singleton a ->
           (match int_of_string_opt (AbsValue.AbsAddr.min_elt a),
                  AbsInterval.singleton_value size with
            | Some base_int, Some z when Z.fits_int z ->
                let n_int = Z.to_int z in
                if n_int > 0 then (
                  let start_offset = base_int - !tmp_addr in
                  let end_offset = start_offset + n_int - 1 in
                  add_chunk start_offset end_offset;
                  add_bulk_range start_offset end_offset
                );
                mem
            | _ -> mem)
       | _ -> mem)
  | _ -> mem

(* bb_name 끝에서부터 '#'로 나눈 뒤, 뒤에서부터 처음 나오는 숫자 조각을 찾는다.
   "...#entry#3"      -> 3
   "...#entry"        -> 0  (숫자 접미사가 없으면 0)
   "...#BB_x.i#4#prune_false" -> 4  (끝에 숫자 아닌 조각이 붙어도 건너뛰고 찾음) *)
   let bb_num_of_bb_name (bb_name : string) : int =
    let parts = List.rev (String.split_on_char '#' bb_name) in
    let rec find = function
      | [] -> 0
      | p :: rest ->
        (match int_of_string_opt p with
         | Some n -> n
         | None -> find rest)
    in
    find parts
  
  let heap_base_start = 30000
  let heap_base_stride = 1000
  
  let get_heap_base (bb_name : string) : int =
    heap_base_start + heap_base_stride * (bb_num_of_bb_name bb_name)

  (* size(AbsValue, interval 도메인)로부터 malloc이 잡을 바이트 크기를 정한다.
    - 유한 구간(singleton 포함) -> max
    - top/무한 등 확정 불가 -> 10000 *)
  let resolve_malloc_size (n : AbsValue.t) : int =
    match n with
    | AbsValue.AbsInt (AbsInterval.IntInterval {min = AbsInterval.I _; max = AbsInterval.I z}) ->
        Z.to_int z
    | _ -> 10000
 
let model_malloc (bb_name : string) (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  let base = get_heap_base bb_name in
  let size =
    match args with
    | [n_e] -> resolve_malloc_size (abs_eval n_e mem)
    | _ -> 10000
  in
  let num_slots = max 1 size in
  let cell_addrs = List.init num_slots (fun i -> string_of_int (base + i)) in
  let mem' =
    List.fold_left (fun mem addr -> AbsMemory.update addr AbsValue.bot mem)
      mem cell_addrs
  in
  let base_val = AbsValue.alpha_int (Z.of_int base) in
  AbsMemory.update ret_addr base_val mem'

let model_calloc (bb_name : string) (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  let base = get_heap_base bb_name in
  let size =
    match args with
    | [nmemb_e; size_e] ->
        let n = resolve_malloc_size (abs_eval nmemb_e mem) in
        let s = resolve_malloc_size (abs_eval size_e mem) in
        min 10000 (n * s)
    | _ -> 10000
  in
  let num_slots = max 1 size in
  let cell_addrs = List.init num_slots (fun i -> string_of_int (base + i)) in
  let mem' =
    List.fold_left (fun mem addr -> AbsMemory.update addr (AbsValue.alpha_int Z.zero) mem)
      mem cell_addrs
  in
  let base_val = AbsValue.alpha_int (Z.of_int base) in
  AbsMemory.update ret_addr base_val mem'

(* let model_malloc _ (mem : AbsMemory.t) : AbsMemory.t =
  let name2 = "addr2" in 
  let name3 = "addr3" in 
  let obj_addr2 = obj_addr_of_name name2 in 
  let obj_addr3 = obj_addr_of_name name3 in
  let addr_name2_val = AbsValue.alpha (AddrLiteral obj_addr2) name2 in  
  let addr_name3_val = AbsValue.alpha (AddrLiteral obj_addr3) name3 in 
  let mem' = AbsMemory.update obj_addr2 addr_name3_val mem in 
  let ret = obj_addr_of_name "ret" in 
  let mem'' = AbsMemory.update ret addr_name2_val mem' in
  mem'' *)

  (* let obj_addr = obj_addr_of_name name in
  let ptr_v = AbsValue.alpha (AddrLiteral obj_addr) "s" in
  let mem' = AbsMemory.update obj_addr AbsValue.bot mem in
  let _ = Env.add "ret" obj_addr !Env.env in
  AbsMemory.update obj_addr ptr_v mem' *)

(* free(p): p가 가리키는 시작 주소에 bot을 할당한다.
malloc이 base 주소를 만들어 돌려주는 것의 역연산.
- p가 AbsAddr이면 집합의 각 주소를 그대로 bot으로 만든다
  (IntToPtr을 거쳐 온 경우, 보통 {base} singleton).
- p가 AbsInt이면 각 정수를 주소 문자열로 바꿔 bot으로 만든다
  (malloc이 ret_addr에 base를 int로 넣으므로 이 경로도 생긴다). *)
let model_free (args : Expr.t list) (mem : AbsMemory.t) : AbsMemory.t =
  let p_e =
    match args with
    | p :: _ -> p
    | [] -> failwith "model_free: unexpected number of args"
  in
  let p = abs_eval p_e mem in
  (* let _ = Format.printf "[free] %a -> bot@." AbsValue.pp p in *)
  match p with
  | AbsValue.AbsAddr a ->
      AbsValue.AbsAddr.fold
        (fun addr mem -> AbsMemory.update addr AbsValue.bot mem)
        a mem
  | AbsValue.AbsInt i ->
      AbsValue.AbsInt.fold
        (fun n mem ->
          let addr = AbsValue.AbsInt.to_string n in
          AbsMemory.update addr AbsValue.bot mem)
        i mem
  | _ -> mem


let abs_interp_term' (bb_name: string) (term : Term.t) (mem : AbsMemory.t) =
    if mem = AbsMemory.bot then mem else
    match term with
    | Br _ -> mem
    | CondBr _ -> mem
    | Ret {ret; _} ->
      let res = abs_eval ret mem in
      let addr = Env.find ret_addr !Env.env in
      AbsMemory.update addr res mem
    | Exit _ -> mem
    (* | CallSite _ -> mem *)
    | CallSite {callee; args; _} ->
      let mem = AbsMemory.update ret_addr AbsValue.top mem in
      if contains_substring callee "memset" then model_memset args mem
      else if contains_substring callee "memcpy" then model_memcpy args mem
      else if contains_substring callee "malloc" then model_malloc bb_name args mem 
      else if contains_substring callee "calloc" then model_calloc bb_name args mem 
      (* else if contains_substring callee "realloc" then model_realloc bb_name name mem *)
      else if contains_substring callee "free" then model_free args mem
      else mem
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

  let seed_argv_addrs (mem : AbsMemory.t) : AbsMemory.t =
    let magic = !tmp_addr in
    let rec loop mem i =
      if i > 100 then mem
      else
        let addr = magic + (i * 4) in
        let mem = AbsMemory.update (string_of_int addr) AbsValue.top mem in
        loop mem (i + 1)
    in
    loop mem 1

let transfer (bb : Basicblock.t) (mem : AbsMemory.t)  =
    let mem' = List.fold_left
    (fun mem stmt ->
        let mem'' = abs_interp_stmt stmt mem in
        mem''
    )
    mem bb.stmts 
    in
    let mem' = abs_interp_term' bb.bb_name bb.term mem' in
    (* let _ = Format.printf "AFTER %a@." AbsMemory.pp mem' in
    let _ = Format.printf "%s@." bb.bb_name in
    let _ = Format.printf "%a@." AbsMemory.pp mem' in *)
    mem'
