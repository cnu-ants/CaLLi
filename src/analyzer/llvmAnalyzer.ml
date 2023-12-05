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

      type ctxtty = Ctxt.t
      type elt = Basicblock.t * ctxtty
      type t = elt list
      let empty = []
      let add w wl = w::wl
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

    let mem = M.mem
    let find = M.find

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
              let widen_mem = 
                if LoopCounter.widen bb_ctxt 
                  then AbsMem.widen prev_mem memory
                  else joined_mem 
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
        let next = Icfg.next bb ctxt mem' !icfg !llmodule in
        let res = List.map (fun bb_ctxt -> (bb_ctxt, mem')) next in
        let wl'', states' = update res wl' states in
        analyze' wl'' states'
    in
    (let main = Module.main !llmodule in
    let entry = Bbpool.find (main.function_name^"#"^"entry") !Bbpool.pool in
    let init_wl = Worklist.add (entry, Ctxt.empty ()) Worklist.empty in
    analyze' init_wl states)

    (** initilizing llmodule and icfg. must be called before ```analyze``` function called *)
    let init llm = 
      let _ = llmodule := llm in
      let _ = icfg := Icfg.make llm in
      ()

      
 end


