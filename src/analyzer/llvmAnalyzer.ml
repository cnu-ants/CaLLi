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

  (* summary stores out-states *)
  let summary = ref States.empty

  let init_module_and_icfg (llm : Module.t) : unit =
    llmodule := llm.function_map;
    icfg := Icfg.make llm.function_map

  let init (llm : Module.t) : AbsMem.t =
    init_module_and_icfg llm;
    List.fold_left
      (fun mem (v : Global.t) -> TF.abs_interp_global v mem)
      AbsMem.empty llm.globals

  let analyze_one
    (bb : Basicblock.t)
    (ctxt : Ctxt.t)
    (wl' : Worklist.t)
    (states : States.t)
    : Worklist.t * States.t * AbsMem.t =
    let in_mem =
      match States.find_mem_opt (bb, ctxt) states with
      | Some m -> m
      | None -> AbsMem.bot
    in

    let out_mem = TF.transfer bb in_mem in
    summary := States.update (bb, ctxt) out_mem !summary;

    let next : (Basicblock.t * Ctxt.t) list =
      Icfg.next bb ctxt out_mem !icfg !llmodule
    in

    let wl'', states'' =
      List.fold_left
        (fun (w, s) ((succ : Basicblock.t), ctxt2) ->
          match States.find_mem_opt (succ, ctxt2) s with
          | Some old_in ->
              if AbsMem.(out_mem <= old_in) then
                (w, s)
              else
                let joined_in = AbsMem.(join old_in out_mem) in
                LoopCounter.update (succ, ctxt2);
                let next_in =
                  if LoopCounter.widen (succ, ctxt2) then
                    AbsMem.widen old_in joined_in
                  else
                    joined_in
                in
                (Worklist.add (succ, ctxt2) w,
                 States.update (succ, ctxt2) next_in s)
          | None ->
              if out_mem = AbsMem.bot then
                (w, s)
              else
                (Worklist.add (succ, ctxt2) w,
                 States.update (succ, ctxt2) out_mem s))
        (wl', states) next
    in
    (wl'', states'', out_mem)

  let analyze_full (entry : Basicblock.t) (states : States.t) : States.t =
    let entry_ctxt = Ctxt.empty () in
    let states =
      match States.find_mem_opt (entry, entry_ctxt) states with
      | Some _ -> states
      | None -> States.update (entry, entry_ctxt) AbsMem.empty states
    in
    let rec analyze' wl states =
      if Worklist.is_empty wl then states
      else
        let bb, ctxt = Worklist.next wl in
        let wl' = Worklist.pop wl in
        let wl'', states'', _out = analyze_one bb ctxt wl' states in
        analyze' wl'' states''
    in
    let init_wl = Worklist.add (entry, entry_ctxt) Worklist.empty in
    analyze' init_wl states
end
