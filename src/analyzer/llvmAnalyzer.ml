module F = Format

module Make
  (AbsVal : AbstractDomain.S)
  (AbsMem : AbstractMemory.S with type valty = AbsVal.t)
  (Ctxt : Context.S with type memty = AbsMem.t)
  (States : States.S with type ctxtty = Ctxt.t and type memty = AbsMem.t)
  (TF : AbstractSemantics.S with type memty = AbsMem.t) =
struct
  module Icfg = Icfg.Make (AbsMem) (Ctxt)

  module Worklist = struct
    exception No_more_basicblock

    type elt = Basicblock.t * Ctxt.t
    type t = elt list

    let empty = []
    let add w wl = wl @ [ w ]
    let is_empty = function [] -> true | _ -> false
    let next = function h :: _ -> h | _ -> raise No_more_basicblock
    let pop = function _ :: t -> t | _ -> raise No_more_basicblock

    let ctxt_to_string (ctxt : Ctxt.t) : string = Format.asprintf "%a" Ctxt.pp ctxt

    let elt_to_string ((bb, ctxt) : elt) : string = bb.bb_name ^ " >> " ^ ctxt_to_string ctxt

    let to_string_list (wl : t) : string list = List.map elt_to_string wl
  end

  module LoopCounter = struct
    module M = Map.Make (struct
      type t = Worklist.elt
      let compare = compare
    end)

    let empty : int M.t = M.empty
    let max_count : int ref = ref 30
    let set_max_count i = max_count := i

    let lc = ref empty
    let mem = M.mem
    let find = M.find

    let update bb_ctxt =
      lc :=
        if mem bb_ctxt !lc then M.add bb_ctxt ((find bb_ctxt !lc) + 1) !lc
        else M.add bb_ctxt 1 !lc

    let widen bb_ctxt = find bb_ctxt !lc > !max_count
  end

  let llmodule = ref Module.empty
  let icfg = ref Icfg.empty
  let summary = ref States.empty

  let icfg_json_cache : string option ref = ref None

  type runtime = {
    entry : Basicblock.t;
    mutable wl : Worklist.t;
    mutable states : States.t;

    (* bb_name -> (ctxt_string, mem_string) list *)
    mem_view : (string, (string * string) list) Hashtbl.t;
  }

  let get_icfg_json () : string =
    match !icfg_json_cache with
    | Some s -> s
    | None -> "{}"

  let init_module_and_icfg (llm : Module.t) : unit =
    llmodule := llm.function_map;
    icfg := Icfg.make llm.function_map;
    icfg_json_cache := Some (Icfg.to_graph_json llm.function_map !icfg |> Yojson.Safe.to_string)

  let init (llm : Module.t) : AbsMem.t =
    init_module_and_icfg llm;
    List.fold_left
      (fun mem (v : Global.t) -> TF.abs_interp_global v mem)
      AbsMem.empty llm.globals

  let init_runtime ~(entry : Basicblock.t) ~(init_states : States.t) : runtime =
    {
      entry;
      wl = Worklist.add (entry, Ctxt.empty ()) Worklist.empty;
      states = init_states;
      mem_view = Hashtbl.create 256;
    }

  let update_mem_view (r : runtime) (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) : unit =
    let bbk = bb.bb_name in
    let ck = Worklist.ctxt_to_string ctxt in
    let mk = Format.asprintf "%a" AbsMem.pp mem in
    let prev = match Hashtbl.find_opt r.mem_view bbk with Some x -> x | None -> [] in
    let prev = List.filter (fun (c, _) -> not (String.equal c ck)) prev in
    Hashtbl.replace r.mem_view bbk ((ck, mk) :: prev)



module CtxtKey = struct
  type t = Ctxt.t
  let compare = compare
end

module PrevM = Map.Make (CtxtKey)


  let analyze_full (entry : Basicblock.t) (states : States.t) : States.t =
    let rec analyze' wl states =
      if Worklist.is_empty wl then states
      else
        let bb, ctxt = Worklist.next wl in
        let wl' = Worklist.pop wl in
        let preds : Basicblock.t list = Icfg.preds bb !icfg !llmodule in
        let mem : AbsMem.t =
          List.fold_left
            (fun mem pred_bb ->
              match States.find_mem_opt (pred_bb, ctxt) states with
              | Some m -> AbsMem.(join m mem)
              | None -> mem)
            AbsMem.empty preds
        in
        let mem = if bb <> entry && mem = AbsMem.empty then AbsMem.bot else mem in
        summary := States.update (bb, ctxt) mem !summary;

        let mem = TF.transfer bb mem in
        let next : (Basicblock.t * Ctxt.t) list = Icfg.next bb ctxt mem !icfg !llmodule in
let prev_snapshot =
  List.fold_left
    (fun acc (_succ, ctxt2) ->
      if PrevM.mem ctxt2 acc then acc
      else
        let p = States.find_mem_opt (bb, ctxt2) states in
        PrevM.add ctxt2 p acc)
    PrevM.empty next
in

let wl'', states'' =
  List.fold_left
    (fun (w, s) ((succ : Basicblock.t), ctxt2) ->
      let prev_mem_opt = PrevM.find ctxt2 prev_snapshot in
      match prev_mem_opt with
      | Some prev_mem ->
          if AbsMem.(mem <= prev_mem) then (w, s)
          else (
            LoopCounter.update (bb, ctxt2);
            let joined_mem = AbsMem.(join prev_mem mem) in
            let widen_mem =
              if LoopCounter.widen (bb, ctxt2) then AbsMem.widen prev_mem joined_mem
              else joined_mem
            in
            (Worklist.add (succ, ctxt2) w, States.update (bb, ctxt2) widen_mem s))
      | None ->
          let w' = if mem = AbsMem.bot then w else Worklist.add (succ, ctxt2) w in
          (w', States.update (bb, ctxt2) mem s))
    (wl', states) next
in
        analyze' wl'' states''
    in
    let init_wl = Worklist.add (entry, Ctxt.empty ()) Worklist.empty in
    analyze' init_wl states
        
let analyze_one
  (entry : Basicblock.t)
  (bb : Basicblock.t)
  (ctxt : Ctxt.t)
  (wl' : Worklist.t)
  (states : States.t)
  : Worklist.t * States.t * AbsMem.t =
  let preds : Basicblock.t list = Icfg.preds bb !icfg !llmodule in
  let mem : AbsMem.t =
    List.fold_left
      (fun mem pred_bb ->
        match States.find_mem_opt (pred_bb, ctxt) states with
        | Some m -> AbsMem.(join m mem)
        | None -> mem)
      AbsMem.empty preds
  in
  let mem = if bb <> entry && mem = AbsMem.empty then AbsMem.bot else mem in

  (* Keep EXACT behavior: summary stores this mem (as your current code does). *)
  summary := States.update (bb, ctxt) mem !summary;

  let mem = TF.transfer bb mem in
  let next : (Basicblock.t * Ctxt.t) list = Icfg.next bb ctxt mem !icfg !llmodule in

  let prev_snapshot =
  List.fold_left
    (fun acc (_succ, ctxt2) ->
      if PrevM.mem ctxt2 acc then acc
      else
        let p = States.find_mem_opt (bb, ctxt2) states in
        PrevM.add ctxt2 p acc)
    PrevM.empty next
in

let wl'', states'' =
  List.fold_left
    (fun (w, s) ((succ : Basicblock.t), ctxt2) ->
      let prev_mem_opt = PrevM.find ctxt2 prev_snapshot in
      match prev_mem_opt with
      | Some prev_mem ->
          if AbsMem.(mem <= prev_mem) then (w, s)
          else (
            LoopCounter.update (bb, ctxt2);
            let joined_mem = AbsMem.(join prev_mem mem) in
            let widen_mem =
              if LoopCounter.widen (bb, ctxt2) then AbsMem.widen prev_mem joined_mem
              else joined_mem
            in
            (Worklist.add (succ, ctxt2) w, States.update (bb, ctxt2) widen_mem s))
      | None ->
          let w' = if mem = AbsMem.bot then w else Worklist.add (succ, ctxt2) w in
          (w', States.update (bb, ctxt2) mem s))
    (wl', states) next
  in
  (wl'', states'', mem)

let step_once_json (r : runtime) : Yojson.Safe.t =
  if Worklist.is_empty r.wl then `Assoc [ ("type", `String "done") ]
  else
    let bb, ctxt = Worklist.next r.wl in
    let wl' = Worklist.pop r.wl in

    (* run exactly one iteration with the SAME semantics as analyze_full *)
    let wl'', states'', out_mem = analyze_one r.entry bb ctxt wl' r.states in

    (* web view: show the mem you computed at this step *)
    update_mem_view r bb ctxt out_mem;

    r.wl <- wl'';
    r.states <- states'';

    let current_bb = bb.bb_name in
    let current_ctxt = Worklist.ctxt_to_string ctxt in
    let wl_view = Worklist.to_string_list r.wl in

    `Assoc
      [
        ("type", `String "worklist");
        ("bb", `String current_bb);
        ("ctxt", `String current_ctxt);
        ("current", `String (Worklist.elt_to_string (bb, ctxt)));
        ("worklist", `List (List.map (fun s -> `String s) wl_view));
      ]

  let get_state_for_bb_json (r : runtime) (bb_name : string) : Yojson.Safe.t =
    let lst = match Hashtbl.find_opt r.mem_view bb_name with Some x -> x | None -> [] in
    let ctxs =
      lst
      |> List.rev
      |> List.map (fun (ctxt, mem) -> `Assoc [ ("ctxt", `String ctxt); ("mem", `String mem) ])
    in
    `Assoc [ ("bb", `String bb_name); ("contexts", `List ctxs) ]
end
