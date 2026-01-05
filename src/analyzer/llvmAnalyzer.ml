module F = Format

(** 
  Functor building an implementation of the Analyzer
given abstract domain, abstract memory, analysis context, abstract states, abstract semantics.
 *)
module Make 
(AbsVal : AbstractDomain.S)
(AbsMem : AbstractMemory.S with type valty = AbsVal.t) (Ctxt : Context.S with type memty = AbsMem.t) 
(States : States.S with type ctxtty = Ctxt.t and type memty = AbsMem.t) 
(TF : AbstractSemantics.S with type memty = AbsMem.t) 
=
  struct
  
  (** Interprocedural Control Flow Graph module tailored to the user-provided 
  context and abstract memory.*)
  module Icfg = Icfg.Make(AbsMem)(Ctxt)

  (** 
  Worklist for Abstract Interpretation.
  *)
  module Worklist = 
    struct
      exception No_more_basicblock

      type elt = Basicblock.t * Ctxt.t
      type t = elt list
      let empty = []
      let add w wl = wl@[w]
      let is_empty = function [] -> true | _ -> false
      let next = function h::_ -> h | _ -> raise No_more_basicblock
      let pop = function _ :: t -> t | _ -> raise No_more_basicblock
      let pp fmt wl = 
        F.fprintf fmt "[%a]" (F.pp_print_list 
          ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
          (fun fmt (bb_ctxt : Basicblock.t * Ctxt.t) -> 
            let bb, ctxt = bb_ctxt in
            F.fprintf fmt "%s >> %a" bb.bb_name Ctxt.pp ctxt
          )) wl
    end

  (** 
  A loop_counter for widening operations in the analysis loop, visiting the same 
  basicblock for a maximum count of loop_counter before performing widening.
  *)
  module LoopCounter = struct 
  
    module M = Map.Make(struct 
      type t = Worklist.elt
      let compare = compare
    end)

    let empty : int M.t = M.empty

    let max_count : int ref = ref 0

    let set_max_count i : unit =
      max_count := i

    let lc = ref empty

    let find_min_max_value () =
      M.fold (fun _ v (acc1, acc2) -> let max = max v acc1 in let min = min v acc2 in (max, min)) !lc (0, 100)

    let mem = M.mem
    let find = M.find

    let min = ref 1000
    let max = ref 0

    let update bb_ctxt = 
      let _ = lc := 
        if mem bb_ctxt !lc
        then M.add bb_ctxt ((find bb_ctxt !lc) + 1) !lc
        else M.add bb_ctxt 1 !lc
      in ()

    let widen bb_ctxt = 
      find bb_ctxt !lc > !max_count
  end


  let llmodule = ref Module.empty
  let icfg = ref Icfg.empty
  let summary = ref States.empty

(*
  (** analysis loop *)
  let analyze (states) = 
    let rec analyze' (wl:Worklist.t) (states: States.t) =
      let rec update sl wl (states : States.t) =
        match sl with
        | [] -> wl, states
        | (bb_ctxt, memory) :: t -> 
        if States.mem bb_ctxt states then
          let prev_mem = States.find_mem bb_ctxt states in
          begin
            if AbsMem.(memory <= prev_mem) then
              update t wl states
            else
              let joined_mem = AbsMem.(join prev_mem memory) in
              let _ = LoopCounter.update bb_ctxt in
              (* let _ = Format.printf "%d\n" (LoopCounter.find bb_ctxt !LoopCounter.lc) in *)
              let widen_mem = 
                if LoopCounter.widen bb_ctxt
                  then 
                    (* let _ = Format.printf "widen %s@." bb.bb_name in *)
                    AbsMem.widen prev_mem joined_mem
                  else 
                    joined_mem 
              in
              update t (Worklist.add bb_ctxt wl) (States.update bb_ctxt widen_mem states)
          end
        else
          update t (Worklist.add bb_ctxt wl) (States.update bb_ctxt memory states)
      in
      if Worklist.is_empty wl then states
      else
        let bb_ctxt = Worklist.next wl in  
        let wl' = Worklist.pop wl in
        let mem = States.find_mem bb_ctxt states in
        let (bb, ctxt) = bb_ctxt in
        let mem' = TF.transfer bb mem in
        let _ = summary := States.update bb_ctxt mem' !summary in
        let next = Icfg.next_intra bb ctxt mem' !icfg !llmodule in
        let res = List.map (fun bb_ctxt -> (bb_ctxt, mem')) next in
        let wl'', states' = update res wl' states in
        analyze' wl'' states'
    in
    (let main = Module.main !llmodule in
    let entry = Bbpool.find (main.entry) !Bbpool.pool in
    let init_wl = Worklist.add (entry, Ctxt.empty ()) Worklist.empty in
    analyze' init_wl states)


*)
(*
  module Worklist_modified = 
    struct
      exception No_more_basicblock

      type t = Basicblock.t list
      let empty = []
      let add w wl = w::wl
      let is_empty = function [] -> true | _ -> false
      let next = function h::_ -> h | _ -> raise No_more_basicblock
      let pop = function _ :: t -> t | _ -> raise No_more_basicblock
      let pp fmt wl = 
        F.fprintf fmt "[%a]" (F.pp_print_list 
          ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
          (fun fmt (bb : Basicblock.t) -> 
            F.fprintf fmt "%s" bb.bb_name
          )) wl
    end
*)

(** analysis loop *)

(*
let analyze states = 
  let rec analyze' wl states = 
    if Worklist.is_empty wl then 
      states
    else
      let bb, ctxt = Worklist.next wl in
      let wl' = Worklist.pop wl in
      let preds : Basicblock.t list = Icfg.preds_intra bb !icfg !llmodule in
      let works : AbsMem.t list = 
        List.fold_left
        (fun lst pred_bb -> 
          match States.find_mem_option (pred_bb, ctxt) states with
          | Some m -> m::lst
          | None -> lst
        )
        [] preds in
      (* calc memory and update context*)
      let res : (Ctxt.t * AbsMem.t) list = 
        List.map
        (fun mem -> 
          let mem' = TF.transfer bb mem in
          
          let ctxt' = Ctxt.apply' bb ctxt mem' in
          (ctxt', mem')
        )
        works in

      
      let succs = Icfg.next_fallback bb !llmodule in
      (* update states of bb with updated context and calculated momory *)
      let wl', states' = 
        List.fold_left
        (fun (w, s) (ctxt, mem) -> 
          match States.find_mem_option (bb, ctxt) s with
          | Some prev_mem -> 
            if AbsMem.(mem <= prev_mem) then
              w, s
            else
              let _ = LoopCounter.update (bb, ctxt) in
              let joined_mem = AbsMem.(join prev_mem mem) in
              let widen_mem = 
                if LoopCounter.widen (bb, ctxt)
                  then 
                    AbsMem.widen prev_mem joined_mem
                  else 
                    joined_mem 
              in
              let w' = 
                List.fold_left
                (fun w' succ -> Worklist.add (succ, ctxt) w')
                w succs in
              let s' = States.update (bb, ctxt) widen_mem s in
              (w', s')
          | None -> 
            let w' = 
              List.fold_left
              (fun w' succ -> Worklist.add (succ, ctxt) w')
              w succs in
            let s' = States.update (bb, ctxt) mem s in
            (w', s')
        )
        (wl', states) res in  
      analyze' wl' states'
  in
  (let main = Module.main !llmodule in
  let entry = Bbpool.find (main.entry) !Bbpool.pool in
  let init_wl = Worklist.add (entry, Ctxt.empty ())  Worklist.empty in
  analyze' init_wl states)
*)

(** analysis loop *)
let analyze entry states = 
  let rec analyze' wl states = 
    if Worklist.is_empty wl then 
      states
    else
      let bb, ctxt = Worklist.next wl in
      let wl' = Worklist.pop wl in
      let preds : Basicblock.t list = Icfg.preds_intra bb !icfg !llmodule in
      let mem : AbsMem.t = 
        List.fold_left
        (fun mem pred_bb -> 
          match States.find_mem_option (pred_bb, ctxt) states with
          | Some m ->  
            AbsMem.(join m mem)
          | None -> mem
        )
        AbsMem.empty preds in
      let mem = if bb != entry && mem = AbsMem.empty then AbsMem.bot else mem in 
      let _ = summary := States.update (bb, ctxt) mem !summary in
      (* calc memory and context*)
      let mem = TF.transfer bb mem in
      (*let _ = Format.printf "%s, %a\n@." bb.bb_name Ctxt.pp ctxt in
      let _ = Format.printf "%a\n@." AbsMem.pp mem in
      *)let next : (Basicblock.t * Ctxt.t) list = Icfg.next_intra bb ctxt mem !icfg !llmodule in
      (* update states of bb with updated context and calculated momory *)
      let wl', states' = 
        List.fold_left
        (fun (w, s) ((succ : Basicblock.t), ctxt) -> 
          let prev_mem = States.find_mem_option (bb, ctxt) states in
          match prev_mem with
          | Some prev_mem -> 
            if AbsMem.(mem <= prev_mem) then
              w, s
            else
              let _ = LoopCounter.update (bb, ctxt) in
              let joined_mem = AbsMem.(join prev_mem mem) in
              let widen_mem = 
                if LoopCounter.widen (bb, ctxt)
                  then
                    AbsMem.widen prev_mem joined_mem
                  else 
                    joined_mem 
              in
              let w' = Worklist.add (succ, ctxt) w in
              let s' = States.update (bb, ctxt) widen_mem s in
              (w', s')
          | None ->
            let w' = 
              if mem = AbsMem.bot then w else 
               Worklist.add (succ, ctxt) w in
            let s' = States.update (bb, ctxt) mem s in
            (w', s')
        )
        (wl', states) next in  
      analyze' wl' states'
  in
  (
  (*let main = Module.main !llmodule in
  let entry = Bbpool.find (main.entry) !Bbpool.pool in *)
  let init_wl = Worklist.add (entry, Ctxt.empty ())  Worklist.empty in
  analyze' init_wl states)


  (** initilizing llmodule and icfg. must be called before ```analyze``` function called *)
  let init (llm : Module.t) = 
  let _ = llmodule := llm.function_map in
  let _ = icfg := Icfg.make llm.function_map in

  let mem = 
    List.fold_left
    (fun mem (v : Global.t) ->
      TF.abs_interp_global v mem
    )
    AbsMem.empty llm.globals in
  mem

      
 end


