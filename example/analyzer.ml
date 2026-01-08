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

module M = Map.Make(struct 
  type t = AbsValue.t
  let compare = compare
end)

module S = Set.Make(String)

let v_map = ref M.empty


let pp_states fmt s = 
  Env.iter (fun var addr ->
     let exit_bb = Bbpool.find "Func_main(i32%arg_esp)#exit" !Bbpool.pool in
     let absMem = States.find_mem (exit_bb, MyContext.empty ()) s in 
     let abs_v = AbsMemory.find addr absMem in
     let set_var = if M.mem abs_v !v_map then M.find abs_v !v_map else S.empty in
     let set_updated = S.add var set_var in 
     let _ = v_map := M.add abs_v set_updated !v_map in 
     Format.fprintf fmt "%s -> %a\n" var AbsValue.pp abs_v 
  ) !Env.env

let pp_v_map fmt v_map = 
  M.iter (fun abs_v set_var -> 
    let _ = Format.printf "%a {\n" AbsValue.pp abs_v in 
    let _ = S.iter (fun var ->
      Format.printf "%s\n" var  
    ) set_var in
    let _ = Format.printf "}\n\n" in ()
  ) v_map

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
  let _ = Format.printf "%a\n" pp_states s in
(*  let _ = Format.printf "%a\n" pp_v_map !v_map in *)
  ()

