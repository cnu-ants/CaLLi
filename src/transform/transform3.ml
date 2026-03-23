module NameMap = Map.Make (String)
module NameSet = Set.Make (String)

let calc_name (e : Expr.t) : string option =
  match e with
  | Expr.Var { name; _ } -> Some name
  | _ -> None

let negate_cond (c : Cond.t) : Cond.t =
  match c with
  | Eq -> Ne
  | Ne -> Eq
  | Ugt -> Ule
  | Uge -> Ult
  | Ult -> Uge
  | Ule -> Ugt
  | Sgt -> Sle
  | Sge -> Slt
  | Slt -> Sge
  | Sle -> Sgt

let build_defmap (func : Function.t) : Stmt.t NameMap.t =
  Cfg.fold
    (fun bb_name _ acc ->
      let bb = Bbpool.find bb_name !Bbpool.pool in
      let _ = Format.printf "build_Defmap@." in
      List.fold_left
        (fun acc (stmt : Stmt.t) ->
          match stmt.inst with
          | Inst.Load { name; _ } -> NameMap.add name stmt acc
          | Inst.ICmp { name; _ } -> NameMap.add name stmt acc
          | _ -> acc)
        acc
        bb.stmts)
    func.cfg
    NameMap.empty

let rec collect_until_load_from_expr
    (defmap : Stmt.t NameMap.t)
    (visited : NameSet.t)
    (e : Expr.t) : Inst.prune_ty list * NameSet.t =
  match calc_name e with
  | None -> ([], visited)
  | Some name -> collect_until_load_from_name defmap visited name

and collect_until_load_from_name
    (defmap : Stmt.t NameMap.t)
    (visited : NameSet.t)
    (name : string) : Inst.prune_ty list * NameSet.t =
  if NameSet.mem name visited then
    ([], visited)
  else
    let visited = NameSet.add name visited in
    match NameMap.find_opt name defmap with
    | None -> ([], visited)
    | Some stmt ->
        begin
          match stmt.inst with
          | Inst.Load { operand; _ } ->
              ([ Inst.Pointer { lhs = name; operand } ], visited)

          | Inst.ICmp { cond; operand0; operand1; _ } ->
              let facts0, visited = collect_until_load_from_expr defmap visited operand0 in
              let facts1, visited = collect_until_load_from_expr defmap visited operand1 in
              (Inst.Predicate { cond; operand0; operand1 } :: (facts0 @ facts1), visited)

          | _ ->
              ([], visited)
        end

let collect_branch_facts
    (defmap : Stmt.t NameMap.t)
    (cond_name : string)
    (cond_ty : Type.t)
    (truth : bool) : Inst.prune_ty list =
  let root_value =
    Expr.ConstInt { ty = cond_ty; value = Z.of_int (if truth then 1 else 0) }
  in
  let root_fact = Inst.Value { lhs = cond_name; rhs = root_value } in
  match NameMap.find_opt cond_name defmap with
  | None ->
      [ root_fact ]

  | Some stmt ->
      begin
        match stmt.inst with
        | Inst.ICmp { cond; operand0; operand1; _ } ->
            let cond = if truth then cond else negate_cond cond in
            let pred_fact = Inst.Predicate { cond; operand0; operand1 } in
            let facts0, visited =
              collect_until_load_from_expr defmap (NameSet.singleton cond_name) operand0
            in
            let facts1, _ =
              collect_until_load_from_expr defmap visited operand1
            in
            root_fact :: pred_fact :: (facts0 @ facts1)

        | Inst.Load { operand; _ } ->
            [ root_fact; Inst.Pointer { lhs = cond_name; operand } ]

        | _ ->
            [ root_fact ]
      end

let collect_switch_case_facts
    (defmap : Stmt.t NameMap.t)
    (cond_name : string)
    (case_value : Expr.t) : Inst.prune_ty list =
  let root_fact = Inst.Value { lhs = cond_name; rhs = case_value } in
  let tail, _ =
    collect_until_load_from_name defmap (NameSet.singleton cond_name) cond_name
  in
  root_fact :: tail

let collect_switch_default_prefix
    (defmap : Stmt.t NameMap.t)
    (cond_name : string) : Inst.prune_ty list =
  let tail, _ =
    collect_until_load_from_name defmap (NameSet.singleton cond_name) cond_name
  in
  tail

let mk_prune_stmts (bb_name : string) (facts : Inst.prune_ty list) : Stmt.t list =
  List.mapi
    (fun index fact ->
      ({ bb_name; index; inst = Inst.Prune fact; loc = None } : Stmt.t))
    facts

let add_prune_node_in_function (f : Function.t) : Function.t =
  let defmap = build_defmap f in
  let cfg =
    Cfg.fold
      (fun bb_name _ cfg ->
        let _ = Format.printf "add prune %s@." bb_name in
        let bb : Basicblock.t = Bbpool.find_bb bb_name in
        let _ = Format.printf "add prune %a>@." Basicblock.pp bb in
        match bb.term with
        | Some Term.CondBr { cond; succ0; succ1; _ } ->
            begin
              match cond, succ0, succ1 with
              | Expr.Var { name = cond_name; _ },
                Expr.BasicBlock { name = name0; _ },
                Expr.BasicBlock { name = name1; _ } ->
                  let _ = Format.printf "add prune %s1@." bb_name in
                  let true_bb : Basicblock.t = Bbpool.find name0 !Bbpool.pool in
                  let false_bb : Basicblock.t = Bbpool.find name1 !Bbpool.pool in

                  let true_prune_bb_name = bb.bb_name ^ "#prune_true" in
                  let false_prune_bb_name = bb.bb_name ^ "#prune_false" in

                  let true_facts =
                    collect_branch_facts defmap cond_name (Expr.get_type cond) true
                  in
                  let false_facts =
                    collect_branch_facts defmap cond_name (Expr.get_type cond) false
                  in

                  let true_stmts = mk_prune_stmts true_prune_bb_name true_facts in
                  let false_stmts = mk_prune_stmts false_prune_bb_name false_facts in

                  let true_prune_bb : Basicblock.t =
                    {
                      func_name = bb.func_name;
                      bb_name = true_prune_bb_name;
                      stmts = true_stmts;
                      term = Some (Term.Br { bb_name = true_prune_bb_name; succ = succ0 });
                      loc = "";
                    }
                  in

                  let false_prune_bb : Basicblock.t =
                    {
                      func_name = bb.func_name;
                      bb_name = false_prune_bb_name;
                      stmts = false_stmts;
                      term = Some (Term.Br { bb_name = false_prune_bb_name; succ = succ1 });
                      loc = "";
                    }
                  in

                  let current_bb : Basicblock.t =
                    {
                      bb with
                      term =
                        Some
                          (Term.CondBr
                             {
                               bb_name;
                               cond;
                               succ0 = Expr.BasicBlock { name = true_prune_bb.bb_name };
                               succ1 = Expr.BasicBlock { name = false_prune_bb.bb_name };
                             });
                    }
                  in

                  let _ = Bbpool.pool := Bbpool.add bb_name current_bb !Bbpool.pool in
                  let _ =
                    Bbpool.pool :=
                      Bbpool.add true_prune_bb.bb_name true_prune_bb !Bbpool.pool
                  in
                  let _ =
                    Bbpool.pool :=
                      Bbpool.add false_prune_bb.bb_name false_prune_bb !Bbpool.pool
                  in

                  let cfg' =
                    Cfg.add bb_name [ true_prune_bb.bb_name; false_prune_bb.bb_name ] cfg
                  in
                  let cfg' = Cfg.add true_prune_bb.bb_name [ true_bb.bb_name ] cfg' in
                  let cfg' = Cfg.add false_prune_bb.bb_name [ false_bb.bb_name ] cfg' in
                  cfg'

              | _ ->
                  cfg
            end

        | Some (Term.Switch { cond; succ; default_succ; _ } as term) ->
            begin
              match cond with
              | Expr.Var { name = cond_name; _ } ->
                  let v_list, bb_list = List.split succ in

                  let prune_bb_list : Basicblock.t list =
                    List.map2
                      (fun (next_bb : Expr.t) (case_value : Expr.t) ->
                        let prune_bb_name =
                          bb_name ^ "#prune_" ^ Format.asprintf "%a" Expr.pp case_value
                        in
                        let facts =
                          collect_switch_case_facts defmap cond_name case_value
                        in
                        let stmts = mk_prune_stmts prune_bb_name facts in
                        let bb' : Basicblock.t =
                          {
                            func_name = bb.func_name;
                            bb_name = prune_bb_name;
                            stmts;
                            term = Some (Term.Br { bb_name = prune_bb_name; succ = next_bb });
                            loc = "";
                          }
                        in
                        let _ = Bbpool.pool := Bbpool.add prune_bb_name bb' !Bbpool.pool in
                        bb')
                      bb_list
                      v_list
                  in

                  let default_bb_name = bb_name ^ "#prune_default" in
                  let default_prefix =
                    collect_switch_default_prefix defmap cond_name
                  in
                  let default_prefix_stmts =
                    mk_prune_stmts default_bb_name default_prefix
                  in
                  let default_stmt : Stmt.t =
                    {
                      bb_name = default_bb_name;
                      index = List.length default_prefix_stmts;
                      inst = Inst.NPrune { cond = cond_name; value = v_list };
                      loc = None;
                    }
                  in

                  let default_bb : Basicblock.t =
                    {
                      func_name = bb.func_name;
                      bb_name = default_bb_name;
                      stmts = default_prefix_stmts @ [ default_stmt ];
                      term = Some (Term.Br { bb_name = default_bb_name; succ = default_succ });
                      loc = "";
                    }
                  in

                  let _ = Bbpool.pool := Bbpool.add default_bb_name default_bb !Bbpool.pool in

                  let current_bb : Basicblock.t =
                    {
                      bb with
                      term =
                        Some
                          (Term.Switch
                             {
                               bb_name;
                               cond;
                               default_succ = Expr.BasicBlock { name = default_bb.bb_name };
                               succ =
                                 List.map2
                                   (fun v (bb : Basicblock.t) ->
                                     (v, Expr.BasicBlock { name = bb.bb_name }))
                                   v_list
                                   prune_bb_list;
                             });
                    }
                  in

                  let _ = Bbpool.pool := Bbpool.add bb_name current_bb !Bbpool.pool in

                  let cfg' =
                    Cfg.add
                      bb_name
                      (default_bb.bb_name :: List.map (fun (bb : Basicblock.t) -> bb.bb_name) prune_bb_list)
                      cfg
                  in

                  let cfg' =
                    List.fold_left2
                      (fun cfg (prune_bb : Basicblock.t) (next_bb : Expr.t) ->
                        Cfg.add
                          prune_bb.bb_name
                          [
                            match next_bb with
                            | Expr.BasicBlock { name; } -> name
                            | _ -> failwith "not a label"
                          ]
                          cfg)
                      cfg'
                      (default_bb :: prune_bb_list)
                      (default_succ :: bb_list)
                  in
                  cfg'

              | _ ->
                  cfg
            end

        | _ ->
            cfg)
      f.cfg
      f.cfg
  in
  let _ = Format.printf "add prune done@." in
  { f with cfg }

let add_prune_node (m : Module.t) : Module.t =
  let function_map =
    Module.fold
      (fun s f acc -> Module.add s (add_prune_node_in_function f) acc)
      m.function_map
      m.function_map
  in
  { m with function_map }
