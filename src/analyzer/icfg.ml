module Make (AbsMem : AbstractMemory.S) (Ctxt : Context.S with type memty = AbsMem.t) =
struct
  module M = Map.Make (String)
  type t = (string list) M.t

  let find = M.find
  let empty = M.empty
  let add = M.add
  let iter = M.iter
  let fold = M.fold
  let pp _ _ = ()

  let make (m : Function.t Module.M.t) : t =
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
          | CallSite { callee; _ } ->
              let f = Module.find bb.func_name m in
              let icfg =
                if Bbpool.mem (callee ^ "#entry") !Bbpool.pool then
                  add bb_name ((callee ^ "#entry") :: (M.find bb_name f.cfg)) icfg
                else icfg
              in
              let next = List.nth (Cfg.next bb f.cfg) 0 in
              if Bbpool.mem (callee ^ "#exit") !Bbpool.pool then
                add (callee ^ "#exit") (next.bb_name :: (M.find (callee ^ "#exit") icfg)) icfg
              else icfg
          | _ -> icfg)
        icfg icfg
    in
    icfg

  let preds (bb : Basicblock.t) icfg _ : Basicblock.t list =
    M.fold
      (fun from succs acc ->
        if List.exists (fun s -> String.equal s bb.bb_name) succs then (Bbpool.find from !Bbpool.pool) :: acc
        else acc)
      icfg []

  let next_fallback (bb : Basicblock.t) m : Basicblock.t list =
    let f = Module.find bb.func_name m in
    Cfg.next bb f.cfg

  let succ (bb : Basicblock.t) icfg =
    M.find bb.bb_name icfg |> List.map (fun b -> Bbpool.find b !Bbpool.pool)

  let next (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) icfg m : (Basicblock.t * Ctxt.t) list =
    let bb_list = M.find bb.bb_name icfg |> List.map (fun b -> Bbpool.find b !Bbpool.pool) in
    let next_bb_list =
      match bb.term with
      | CallSite _ ->
          let fallbacks = next_fallback bb m in
          List.filter_map (fun v -> if List.mem v fallbacks then None else Some v) bb_list
      | _ -> bb_list
    in
    if next_bb_list <> [] then
      Ctxt.apply bb next_bb_list ctxt mem
    else
      let fallbacks = next_fallback bb m in
      List.map (fun v -> (v, ctxt)) fallbacks

  (* --- JSON helpers --- *)

  let bb_instrs_as_strings (bb : Basicblock.t) : string list =
    (* This is the same trick you used earlier when instruction rendering worked. *)
    let s = Format.asprintf "%a" Basicblock.pp bb in
    s
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (fun x -> x <> "")

  let edge_kind_of (m : Function.t Module.M.t) ~(from_id : string) ~(to_id : string) : string =
    let from_bb = Bbpool.find from_id !Bbpool.pool in
    match from_bb.term with
    | CallSite { callee; _ } ->
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
    let nodes =
      M.fold
        (fun from succs acc ->
          let acc = S.add from acc in
          List.fold_left (fun a s -> S.add s a) acc succs)
        icfg S.empty
    in
    let nodes_json =
      nodes
      |> S.to_list
      |> List.map (fun id ->
             let bb = Bbpool.find id !Bbpool.pool in
             let instrs = bb_instrs_as_strings bb in
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
