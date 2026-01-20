module F = Format
module AbsValue = AbsValue


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

(* 분석 상태 관리 *)
module States = States.Make (MyContext) (AbsMemory)
(* 분석기 *)
module Analyzer = LlvmAnalyzer.Make (AbsValue) (AbsMemory) (MyContext) (States) (TF)
module Summary = States

module M = Map.Make(struct 
  type t = AbsValue.t
  let compare = compare
end)

module S = Set.Make(String)


let _ =
  let _ = Format.printf "Analyze Start@." in
  let init_mem = Analyzer.init (Init.m ())  in
  (* 분석할 함수를 선택함 *)
  let target_f : Function.t = Module.find Sys.argv.(2) (Init.llmodule ()) in
  (*let main : Function.t = Module.main (Init.llmodule ()) in
  let entry = Bbpool.find (main.entry) !Bbpool.pool in*)
  (* 함수에서 entry 블럭을 찾음 *)
  let entry = Bbpool.find (target_f.entry) !Bbpool.pool in
  
  (* 루프 최대 반복횟수 설정 *)
  let _ = Analyzer.LoopCounter.set_max_count 50 in
  let init_states = States.update (entry, MyContext.empty ()) init_mem States.empty in
  let _ = Format.printf "analyze...@." in
  (* 초기 설정하고 entry 함수 가지고 분석 *)
  let s = Analyzer.analyze entry init_states in
  let _ = Format.printf "analyze done...@." in
  let s = !Analyzer.summary in
  (*let _ = Format.printf "%a\n" States.pp s in*)
  let _ = Format.printf "%a\n" States.pp s in
  let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in 
  (*let _ = Format.printf "%a\n" pp_states s in
*)(*  let _ = Format.printf "%a\n" pp_v_map !v_map in *)
  ()

