module F = Format

open Calli

let _ =
  let _ = Format.printf "Init@." in
  let m = Init.init () in
  let m = Init.transform_call () in
  let _ = Init.transform_select () in
  let _ = Init.transform_prune () in
  let _ = Init.make_llm () in
  let _ = Init.make_call_graph () in 
  ()


module MyContext = CallSiteContext.Make(
  struct 
    type t = String.t
    type memty = AbsMemory.t
    let size = ref 0

    let llmodule = Init.llmodule ()
    let call_graph = Init.call_graph ()
    let pp fmt (s : t) = F.fprintf fmt "%s" s
  end
)


module States = States.Make (MyContext) (AbsMemory)
module Analyzer = LlvmAnalyzer.Make (AbsValue) (AbsMemory) (MyContext) (States) (TF)
module Summary = States


let _ =
  let _ = Format.printf "Analyze Start@." in
  let init_mem = Analyzer.init (Init.m ())  in
  let target_f : Function.t = Module.find Sys.argv.(2) (Init.llmodule ()) in
  (*let main : Function.t = Module.main (Init.llmodule ()) in
  let entry = Bbpool.find (main.entry) !Bbpool.pool in*)
  let entry = Bbpool.find (target_f.entry) !Bbpool.pool in
  
  let _ = Analyzer.LoopCounter.set_max_count 10 in
  let init_states = States.update (entry, MyContext.empty ()) init_mem States.empty in
  (*let _ = Format.printf "analyze...\n" in*)
  let s = Analyzer.analyze entry init_states in
  let s = !Analyzer.summary in
  let _ = Format.printf "%a\n" States.pp s in
  let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in
  ()

