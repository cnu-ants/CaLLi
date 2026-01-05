
module S = Set.Make(String)

let s = ref S.empty


let add_ctxt_var (f : Function.t) =   
  let _ = Cfg.iter
    (fun bb_name _ ->
      let bb = Bbpool.find bb_name !Bbpool.pool in
      match bb.term with
      | CondBr {cond; _}
      | Switch {cond; _} -> 
          (match cond with
          | Name {name; _} ->
            let _ = s := S.add name !s in ()
          | _ -> ())
      | _ -> ()
    ) f.cfg
  in
  ()  



let rec collect_fla_vars (f : Function.t) : unit = 
  let prev_s = !s in
  let _ = Cfg.iter
    (fun bb_name _ ->
      let bb = Bbpool.find bb_name !Bbpool.pool in
      List.iter
        (fun (stmt : Stmt.t) ->
              let _ = (match stmt.inst with
              | ICmp {name; operand0; operand1; _} 
              | Select {name; operand0; operand1; _;} 
              | BinaryOp {name; operand0; operand1; _} ->
                 let _ = (match operand0 with
                 | Name {name=name'; _} ->
                   let _ = (if S.mem name' !s then
                     let _ = s := S.add name !s in
                     ()
                   else ())
                   in
                   let _ = (if S.mem name !s then
                     let _ = s := S.add name' !s in
                     ()
                   else ()) in ()
                 | _ -> ()
                 ) in
                 let _ = (match operand1 with
                 | Name {name=name'; _} ->
                   let _ = (if S.mem name' !s then
                     let _ = s := S.add name !s in
                     ()
                   else ())
                   in
                   let _ = (if S.mem name !s then
                     let _ = s := S.add name' !s in
                     ()
                   else ()) in ()
                 | _ -> ()
                 ) in
                 ()
              | Trunc {name; operand; _}
              | Sext {name; operand; _}
              | Zext {name; operand; _} 
              | Store {operand; name; _}
              | Load {name; operand;  _;} -> 
                 let _ = (match operand with
                 | Name {name=name'; _} ->
                   let _ = (if S.mem name' !s then
                     let _ = s := S.add name !s in
                     ()
                   else ())
                   in
                   let _ = (if S.mem name !s then
                     let _ = s := S.add name' !s in
                     ()
                   else ()) in ()
                 | _ -> ()
                 ) in
                 ()
              | _ -> ()
              ) in ()
        ) bb.stmts
    ) f.cfg
  in
  if prev_s = !s then ()
  else collect_fla_vars f 
  
  
  
let prune_unused_stmts (f : Function.t) = 
  let _ = Cfg.iter
    (fun bb_name _ ->
      let bb = Bbpool.find bb_name !Bbpool.pool in
      let pruned_stmts = List.fold_left
        (fun stmts (stmt : Stmt.t) ->
            let pruned_inst = 
              (match stmt.inst with
              | ICmp {name; _} 
              | Select {name;  _;} 
              | BinaryOp {name; _}
              | Trunc {name; _}
              | Sext {name; _}
              | Zext {name;  _} 
              | Alloc {name; _} 
              | Store {name; _}
              | PtrToInt {name; _}
              | IntToPtr {name; _}
              | Load {name; _;} -> 
                if S.mem name !s then
                  stmt.inst
                else Other
              | _ -> stmt.inst
              )
            in
            let s : Stmt.t = {stmt with inst=pruned_inst} in
            stmts@[s]
        )
        [] bb.stmts
      in
      let pruned_term = 
        (match bb.term with
        (*| CondBr _*)
        | Switch _ -> bb.term
        | CondBr {cond; _} -> 
          (try
          let var = Expr.typed_var_of_expr cond in
          let cond = var.name in
          if S.mem cond !s then
            bb.term
          else Other
          with _ -> Other)
        | _ -> Other
        ) in
      let pruned_bb : Basicblock.t = { bb with stmts=pruned_stmts; term=pruned_term;} in
      let _ = Bbpool.pool := Bbpool.add bb_name pruned_bb !Bbpool.pool in
      ()
    ) f.cfg
  in
  ()

let transform_func_fla f var = 
  let _ = add_ctxt_var f in
  let _ = collect_fla_vars f in
  let _ = prune_unused_stmts f in
  ()

let transform_fla (m : Module.t) var : Module.t =
  let _ = 
    Module.iter
    (fun _ v -> transform_func_fla v var)
    m.function_map
  in
  m

  


