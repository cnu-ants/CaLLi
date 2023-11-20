(** Add Prune Node as a first instruction in the next basicblock of the conditional branch *)

let add_prune_node (f : Function.t) = 
  let cfg = 
    Cfg.fold
    (fun bb_name _ cfg ->
      let bb : Basicblock.t = Bbpool.find_bb bb_name in
      match bb.term with
      | CondBr {cond; succ0; succ1; _} ->
        (match cond, succ0, succ1 with
        | Name {name=cond_name;_}, Name {name=name0; _}, Name {name=name1; _} -> 
          let (true_bb : Basicblock.t), (false_bb:Basicblock.t) 
            = (Bbpool.find name0 !Bbpool.pool), (Bbpool.find name1 !Bbpool.pool) in
          let true_value : Expr.t = ConstInt {ty=Expr.get_type cond; value=Z.of_int 1} in
          let false_value : Expr.t = ConstInt {ty=Expr.get_type cond; value=Z.of_int 0} in 
          let true_stmt : Stmt.t = 
            {bb_name=bb.bb_name^"#prune_true"; index=0; inst=Prune{cond=cond_name; value=true_value}} in
          let false_stmt : Stmt.t =
            {bb_name=bb.bb_name^"#prune_false"; index=0; inst=Prune {cond=cond_name; value=false_value}} in
          let true_prune_bb : Basicblock.t = 
            {func_name=bb.func_name; bb_name=true_stmt.bb_name; stmts=[true_stmt]; term=Br{bb_name=true_stmt.bb_name; succ=succ0}; loc=""} in
          let false_prune_bb : Basicblock.t = 
            {func_name=bb.func_name; bb_name=false_stmt.bb_name; stmts=[false_stmt]; term=Br{bb_name=false_stmt.bb_name; succ=succ1}; loc=""} in
          let current_bb : Basicblock.t = 
            {bb with term=CondBr {bb_name=bb_name; cond=cond; succ0=Name{ty=Label; name=true_prune_bb.bb_name}; succ1=Name{ty=Label; name=false_prune_bb.bb_name}}} in 
          let _ = Bbpool.pool := Bbpool.add bb_name current_bb !Bbpool.pool in
          let _ = Bbpool.pool := Bbpool.add true_prune_bb.bb_name true_prune_bb !Bbpool.pool in
          let _ = Bbpool.pool := Bbpool.add false_prune_bb.bb_name false_prune_bb !Bbpool.pool in
          let cfg' = Cfg.add bb_name [true_prune_bb.bb_name; false_prune_bb.bb_name] cfg in
          let cfg' = Cfg.add true_prune_bb.bb_name [true_bb.bb_name] cfg' in
          let cfg' = Cfg.add false_prune_bb.bb_name [false_bb.bb_name] cfg' in
          cfg'
        | _ -> failwith "unreachable") 
      | Switch {cond; succ; default_succ; _;} ->
        let cond_name = (match cond with | Name {name; _;} -> name | _ -> failwith "not a label") in
        let v_list, bb_list = List.split succ in
        let stmt_list : Stmt.t list = 
          List.map
          (fun (v: Expr.t) -> 
            let stmt : Stmt.t = {bb_name=bb_name^"#prune_"^(Format.asprintf "%a" Expr.pp v); index=0; inst=Prune {cond=cond_name; value=v}} in stmt)
          v_list
        in
        let default_stmt : Stmt.t = {bb_name=bb_name^"#prune_default"; index=0; inst=NPrune {cond=cond_name; value=v_list}} in
        let prune_bb_list : Basicblock.t list = 
          List.map2
          (fun (bb' : Expr.t) (stmt : Stmt.t) -> 
            let bb' : Basicblock.t = {func_name=bb.func_name; bb_name=stmt.bb_name; stmts=[stmt]; term=Br{bb_name=stmt.bb_name; succ=bb'}; loc=""} in
            let _ = Bbpool.pool := Bbpool.add stmt.bb_name bb' !Bbpool.pool in
            bb'
          )
          bb_list stmt_list in
        let default_bb : Basicblock.t = 
          {func_name=bb.func_name; bb_name=default_stmt.bb_name; stmts=[default_stmt]; term=Br{bb_name=default_stmt.bb_name; succ=default_succ}; loc=""} in
        let _ = Bbpool.pool := Bbpool.add default_stmt.bb_name default_bb !Bbpool.pool in
        let current_bb = 
          {bb with term=Switch {bb_name=bb_name; cond=cond; default_succ=Name{ty=Label; name=default_bb.bb_name};
          succ=List.map2 (fun v (bb : Basicblock.t) -> v, Expr.Name{ty=Label; name=bb.bb_name}) v_list prune_bb_list}} in
        let _ = Bbpool.pool := Bbpool.add bb_name current_bb !Bbpool.pool in
        let cfg' = Cfg.add bb_name (default_bb.bb_name::(List.map (fun (bb : Basicblock.t) -> bb.bb_name) prune_bb_list)) cfg in
        let cfg' = List.fold_left2
          (fun cfg (prune_bb: Basicblock.t) (next_bb: Expr.t) -> 
            Cfg.add prune_bb.bb_name [match next_bb with | Name {name; _;} -> name |_ -> failwith "not a label"] cfg
          )
          cfg'
          (default_bb::prune_bb_list)
          (default_succ::bb_list) in
        cfg'
      | _ -> cfg
    )
    f.cfg
    f.cfg
  in
  {f with cfg=cfg}


let add_prune_node (m : Module.t) : Module.t =
  let function_map = 
    Module.fold
    (fun s f m-> Module.add s (add_prune_node f) m)
    m.function_map
    m.function_map
  in
  {m with function_map=function_map}
