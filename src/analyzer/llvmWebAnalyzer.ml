module F = Format

module Make
  (AbsVal : AbstractDomain.S)
  (AbsMem : AbstractMemory.S with type valty = AbsVal.t)
  (Ctxt : Context.S with type memty = AbsMem.t)
  (States : States.S with type ctxtty = Ctxt.t and type memty = AbsMem.t)
  (TF : AbstractSemantics.S with type memty = AbsMem.t) =
struct
  module Core = LlvmAnalyzer.Make (AbsVal) (AbsMem) (Ctxt) (States) (TF)
  include Core

  type runtime = {
    entry : Basicblock.t;
    init_states : States.t;
    mutable wl : Worklist.t;
    mutable states : States.t;
    mem_view : (string, (string * AbsMem.t) list) Hashtbl.t;
    mutable last : (Basicblock.t * Ctxt.t) option;
  }

  let init_runtime ~(entry : Basicblock.t) ~(init_states : States.t) : runtime =
    {
      entry;
      init_states;
      wl = Worklist.add (entry, Ctxt.empty ()) Worklist.empty;
      states = init_states;
      mem_view = Hashtbl.create 256;
      last = None;
    }

  let restart (r : runtime) : unit =
    r.wl <- Worklist.add (r.entry, Ctxt.empty ()) Worklist.empty;
    r.states <- r.init_states;
    Hashtbl.clear r.mem_view;
    r.last <- None;
    summary := States.empty;
    LoopCounter.lc := LoopCounter.empty

  let bb_to_func (bb_name : string) : string =
    match String.index_opt bb_name '#' with
    | Some i -> String.sub bb_name 0 i
    | None -> bb_name

  let get_icfg_json () : string =
    Icfg.to_graph_json !llmodule !icfg |> Yojson.Safe.to_string

  let get_functions_json () : Yojson.Safe.t =
    Icfg.to_functions_json !llmodule

  let get_block_index_json () : Yojson.Safe.t =
    Icfg.to_block_index_json !llmodule

  let get_callgraph_full_json () : Yojson.Safe.t =
    CallGraph.to_full_json !llmodule

  let get_callgraph_neighbors_json (focus : string) : Yojson.Safe.t =
    CallGraph.to_neighbors_json !llmodule focus

  let get_callgraph_scc_json () : Yojson.Safe.t =
    CallGraph.to_scc_json !llmodule

  let get_cfg_for_func_json (func_name : string) : Yojson.Safe.t =
    Icfg.to_cfg_json !llmodule func_name

  let update_mem_view (r : runtime) (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) : unit =
    let bbk = bb.bb_name in
    let ck = Worklist.ctxt_to_string ctxt in
    let prev = match Hashtbl.find_opt r.mem_view bbk with Some x -> x | None -> [] in
    let prev = List.filter (fun (c, _) -> not (String.equal c ck)) prev in
    Hashtbl.replace r.mem_view bbk ((ck, mem) :: prev)

  type step_ev = Done | Stepped of string

  let step_once (r : runtime) : step_ev =
    if Worklist.is_empty r.wl then Done
    else
      let bb, ctxt = Worklist.next r.wl in
      let wl' = Worklist.pop r.wl in
      let wl'', states'', out_mem = analyze_one bb ctxt wl' r.states in
      update_mem_view r bb ctxt out_mem;
      r.wl <- wl'';
      r.states <- states'';
      r.last <- Some (bb, ctxt);
      Stepped bb.bb_name

  let snapshot_json (r : runtime) : Yojson.Safe.t =
    match r.last with
    | None ->
        if Worklist.is_empty r.wl then `Assoc [ ("type", `String "done") ]
        else
          let bb, ctxt = Worklist.next r.wl in
          let wl_view = Worklist.to_string_list r.wl in
          `Assoc
            [
              ("type", `String "worklist");
              ("bb", `String bb.bb_name);
              ("ctxt", `String (Worklist.ctxt_to_string ctxt));
              ("current", `String (Worklist.elt_to_string (bb, ctxt)));
              ("worklist", `List (List.map (fun s -> `String s) wl_view));
            ]
    | Some (bb, ctxt) ->
        let wl_view = Worklist.to_string_list r.wl in
        `Assoc
          [
            ("type", `String "worklist");
            ("bb", `String bb.bb_name);
            ("ctxt", `String (Worklist.ctxt_to_string ctxt));
            ("current", `String (Worklist.elt_to_string (bb, ctxt)));
            ("worklist", `List (List.map (fun s -> `String s) wl_view));
          ]

  let state_contexts_json_of_mem_list (lst : (string * AbsMem.t) list) : Yojson.Safe.t list =
    lst
    |> List.rev
    |> List.map (fun (ctxt, mem) ->
           let entries =
             if AbsMem.is_bot mem then []
             else
               AbsMem.bindings mem
               |> List.map (fun (addr, v) ->
                      `Assoc
                        [
                          ("addr", `String addr);
                          ("value", `String (Format.asprintf "%a" AbsVal.pp v));
                        ])
           in
           `Assoc
             [
               ("ctxt", `String ctxt);
               ("is_bot", `Bool (AbsMem.is_bot mem));
               ("entries", `List entries);
             ])

  let get_state_for_bb_json (r : runtime) (bb_name : string) : Yojson.Safe.t =
    let lst = match Hashtbl.find_opt r.mem_view bb_name with Some x -> x | None -> [] in
    let ctxs = state_contexts_json_of_mem_list lst in
    `Assoc [ ("bb", `String bb_name); ("contexts", `List ctxs) ]

  let get_all_states_json (r : runtime) : Yojson.Safe.t =
    let items =
      Hashtbl.fold
        (fun bb_name _ acc -> get_state_for_bb_json r bb_name :: acc)
        r.mem_view
        []
      |> List.rev
    in
    `Assoc [ ("items", `List items) ]

  let get_states_for_func_json (r : runtime) (func_name : string) : Yojson.Safe.t =
    let items =
      Hashtbl.fold
        (fun bb_name lst acc ->
          if String.equal (bb_to_func bb_name) func_name then
            let ctxs = state_contexts_json_of_mem_list lst in
            (`Assoc [ ("bb", `String bb_name); ("contexts", `List ctxs) ]) :: acc
          else acc)
        r.mem_view
        []
      |> List.sort (fun a b ->
             let get_bb = function
               | `Assoc kv -> (
                   match List.assoc_opt "bb" kv with
                   | Some (`String s) -> s
                   | _ -> "")
               | _ -> ""
             in
             String.compare (get_bb a) (get_bb b))
    in
    `Assoc [ ("items", `List items) ]

let get_callgraph_neighbors_meta_json (fn : string) : Yojson.Safe.t =
  CallGraph.to_neighbors_meta_json !llmodule fn
end
