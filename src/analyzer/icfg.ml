module Make (AbsMem : AbstractMemory.S) (Ctxt : Context.S with type memty = AbsMem.t) =
struct
  module M = Map.Make (String)
  type t = (string list) M.t

  let find = M.find
  let empty = M.empty
  let add = M.add
  let iter = M.iter
  let fold = M.fold
  
  let pp _ icfg =
    M.iter
      (fun k v ->
        let _ = Format.printf "%s -> " k in
        let _ = List.iter (fun s -> Format.printf "%s ," s) v in
        Format.printf "\n")
      icfg

  let make (m : Function.t Module.M.t) : t =
    let _ = Format.printf "Make@." in
    let icfg =
      Module.fold
        (fun _ (func : Function.t) icfg -> M.union (fun _ s1 _ -> Some s1) icfg func.cfg)
        m empty
    in
    let icfg =
      M.fold
        (fun bb_name _ icfg ->
          let bb = Bbpool.find bb_name !Bbpool.pool in
          match bb.term with
          | Some CallSite { callee; _ } ->
              let f = Module.find bb.func_name m in
              let f_callee = Module.find_opt callee m in
              let icfg =
                (match f_callee with
                | Some f_callee -> 
                   let icfg' = add bb_name (f_callee.entry :: (M.find bb_name f.cfg)) icfg in
                   let next = List.nth (Cfg.next bb f.cfg) 0 in
                   if Bbpool.mem f_callee.exit !Bbpool.pool then
                     add f_callee.exit (next.bb_name :: (M.find f_callee.exit icfg')) icfg'
                   else icfg'
                | None -> icfg
                )
             in icfg
          | _ -> icfg)
        icfg icfg
    in
    let _ = Format.printf "ICFG : %a" pp icfg in
    let _ = Format.printf "ICFG END@." in
    icfg

  let preds_intra (bb : Basicblock.t) _ m : Basicblock.t list =
    let f = Module.find bb.func_name m in
    Cfg.fold
      (fun bb_name nexts lst ->
        if List.mem bb.bb_name nexts then (Bbpool.find_bb bb_name) :: lst else lst)
      f.cfg []

  let preds (bb : Basicblock.t) icfg _ : Basicblock.t list =
    M.fold
      (fun from succs acc ->
        if List.exists (fun s -> String.equal s bb.bb_name) succs then (Bbpool.find from !Bbpool.pool) :: acc
        else acc)
      icfg []

  let next_fallback (bb : Basicblock.t) m : Basicblock.t list =
    let f = Module.find bb.func_name m in
    Cfg.next bb f.cfg

  let next_intra (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) _ m :
      (Basicblock.t * Ctxt.t) list =
    let f = Module.find bb.func_name m in
    let next_bb_list = Cfg.next bb f.cfg in
    Ctxt.apply bb next_bb_list ctxt mem

  let succ (bb : Basicblock.t) icfg =
    M.find bb.bb_name icfg |> List.map (fun b -> Bbpool.find b !Bbpool.pool)

  let next (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) icfg m : (Basicblock.t * Ctxt.t) list =
    let bb_list = M.find bb.bb_name icfg |> List.map (fun b -> Bbpool.find b !Bbpool.pool) in
    let next_bb_list =
      match bb.term with
      | Some CallSite _ ->
          let fallbacks = next_fallback bb m in
          List.filter_map (fun v -> if List.mem v fallbacks then None else Some v) bb_list
      | _ -> bb_list
    in
    if next_bb_list <> [] then
      Ctxt.apply bb next_bb_list ctxt mem
    else
      let fallbacks = next_fallback bb m in
      List.map (fun v -> (v, ctxt)) fallbacks


  (* ---------- JSON helpers: instruction rendering without using Basicblock.pp/Inst.pp/Term.pp ---------- *)

  let expr_s (e : Expr.t) : string = Format.asprintf "%a" Expr.pp e
  let ty_s (t : Type.t) : string = Format.asprintf "%a" Type.pp t
  let op_s (o : Op.t) : string = Format.asprintf "%a" Op.pp o
  let cond_s (c : Cond.t) : string = Format.asprintf "%a" Cond.pp c

  let inst_to_string (i : Inst.t) : string =
    match i with
    | BinaryOp { name; op; operand0; operand1; _ } ->
        Printf.sprintf "%s = %s %s %s" name (op_s op) (expr_s operand0) (expr_s operand1)
    | Alloc { name; ty } ->
        Printf.sprintf "%s = alloc %s" name (ty_s ty)
    | Store { operand; name; ty } ->
        Printf.sprintf "store %s %s %s" (expr_s operand) (ty_s ty) name
    | Load { name; operand; _ } ->
        Printf.sprintf "%s = load %s" name (expr_s operand)
    | PtrToInt { name; operand; ty } ->
        Printf.sprintf "%s = ptrtoint %s to %s" name (expr_s operand) (ty_s ty)
    | IntToPtr { name; operand; ty } ->
        Printf.sprintf "%s = inttoptr %s to %s" name (expr_s operand) (ty_s ty)
    | ICmp { name; cond; operand0; operand1; _ } ->
        Printf.sprintf "%s = icmp %s %s %s" name (cond_s cond) (expr_s operand0) (expr_s operand1)
    | Select { name; cond; operand0; operand1; _ } ->
        Printf.sprintf "%s = select %s %s %s" name (expr_s cond) (expr_s operand0) (expr_s operand1)
    | ReturnSite { name; ty } ->
        Printf.sprintf "%s = %s(call return)" name (ty_s ty)
    | Call { name; _ } ->
        Printf.sprintf "call %s" name
    | GetElementPtr { name; ty; operand; index } ->
        let idxs = index |> List.map expr_s |> String.concat ", " in
        Printf.sprintf "%s = getelementptr %s %s, %s" name (ty_s ty) (expr_s operand) idxs
    | BitCast { name; operand; ty } ->
        Printf.sprintf "%s = bitcast %s to %s" name (expr_s operand) (ty_s ty)
    | Sext { name; operand; ty } ->
        Printf.sprintf "%s = sext %s to %s" name (expr_s operand) (ty_s ty)
    | Zext { name; operand; ty } ->
        Printf.sprintf "%s = zext %s to %s" name (expr_s operand) (ty_s ty)
    | Trunc { name; operand; ty } ->
        Printf.sprintf "%s = trunc %s to %s" name (expr_s operand) (ty_s ty)
    | Prune { cond; value } ->
        Printf.sprintf "prune %s %s" cond (expr_s value)
    | NPrune { cond; value } ->
        let vs = value |> List.map expr_s |> String.concat ", " in
        Printf.sprintf "!prune %s %s" cond vs
    | Other -> "Other"

  let term_to_string (t : Term.t) : string =
    match t with
    | CallSite { callee; args; _ } ->
        let a = args |> List.map expr_s |> String.concat ", " in
        Printf.sprintf "call %s(%s)" callee a
    | Br { succ; _ } ->
        Printf.sprintf "br %s" (expr_s succ)
    | CondBr { cond; succ0; succ1; _ } ->
        Printf.sprintf "condbr %s %s %s" (expr_s cond) (expr_s succ0) (expr_s succ1)
    | Ret { ret; _ } ->
        Printf.sprintf "ret %s" (expr_s ret)
    | Exit _ ->
        "exit"
    | Switch { cond; default_succ; succ; _ } ->
        let cases =
          succ
          |> List.map (fun (e1, e2) -> Printf.sprintf "%s %s" (expr_s e1) (expr_s e2))
          |> String.concat ", "
        in
        Printf.sprintf "switch %s %s [%s]" (expr_s cond) (expr_s default_succ) cases
    | Other -> "Other"

  let bb_instrs (bb : Basicblock.t) : string list =
    let stmt_lines = bb.stmts |> List.map (fun (s : Stmt.t) -> inst_to_string s.inst) in
    match bb.term with
    | Some term -> stmt_lines @ [ term_to_string term ]
    | None -> stmt_lines

  (* Edge kind classification *)
  let edge_kind_of (m : Function.t Module.M.t) ~(from_id : string) ~(to_id : string) : string =
    let from_bb = Bbpool.find from_id !Bbpool.pool in
    match from_bb.term with
    | Some CallSite { callee; _ } ->
        let f = Module.find from_bb.func_name m in
        let fallbacks = Cfg.next from_bb f.cfg |> List.map (fun (b:Basicblock.t) -> b.bb_name) in
        if String.equal to_id (callee ^ "#entry") then "call"
        else if List.exists (fun x -> String.equal x to_id) fallbacks then "fallback"
        else "intra"
    | _ ->
        if String.length from_id >= 5 && String.sub from_id (String.length from_id - 5) 5 = "#exit" then "ret"
        else "intra"

  let to_graph_json (m : Function.t Module.M.t) (icfg : t) : Yojson.Safe.t =
    let module S = Set.Make (String) in
    let node_ids =
      M.fold
        (fun from succs acc ->
          let acc = S.add from acc in
          List.fold_left (fun a s -> S.add s a) acc succs)
        icfg S.empty
    in

    let nodes_json =
      node_ids
      |> S.to_list
      |> List.map (fun id ->
             let bb = Bbpool.find id !Bbpool.pool in
             let instrs = bb_instrs bb in
             `Assoc
               [
                 ("id", `String id);
                 ("label", `String id);
                 ("instrs", `List (List.map (fun x -> `String x) instrs));
               ])
    in

    let edges_json, _ =
      M.fold
        (fun from succs (acc, idx) ->
          List.fold_left
            (fun (acc2, idx2) to_ ->
              let eid = "e" ^ string_of_int idx2 in
              let kind = edge_kind_of m ~from_id:from ~to_id:to_ in
              ( `Assoc
                  [
                    ("id", `String eid);
                    ("source", `String from);
                    ("target", `String to_);
                    ("kind", `String kind);
                  ]
                :: acc2
              , idx2 + 1 ))
            (acc, idx) succs)
        icfg ([], 0)
    in

    `Assoc [ ("nodes", `List nodes_json); ("edges", `List (List.rev edges_json)) ]
end
