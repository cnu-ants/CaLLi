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
module MyContext2 = CallSiteContext.Make(
  struct 
    type t = String.t
    type memty = AbsMemory2.t
    let size = ref 0

    let llmodule = Init.llmodule ()
    let call_graph = Init.call_graph ()
    let pp fmt (s : t) = F.fprintf fmt "%s" s
  end
)

(* Set Domain  분석기 *)
module States2 = States.Make (MyContext2) (AbsMemory2)
module Analyzer2 = LlvmAnalyzer2.Make (AbsValue2) (AbsMemory2) (MyContext2) (States2) (TF2)
module Summary2 = States2

(* Interval Domain 분석기 *)
module States = States.Make (MyContext) (AbsMemory)
module Analyzer = LlvmAnalyzer.Make (AbsValue) (AbsMemory) (MyContext) (States) (TF)
module Summary = States

module M = Map.Make(struct 
  type t = AbsValue.t
  let compare = compare
end)

module S = Set.Make(String)

let expand_intset (s: AbsIntSet.S.t) : AbsInterval.EltSet.t = 
  let base = AbsIntSet.S.fold
  (fun z acc -> AbsInterval.EltSet.add (AbsInterval.I z) acc)
  s AbsInterval.EltSet.empty in 
  let base = AbsInterval.EltSet.add AbsInterval.MinInf base in 
  let base = AbsInterval.EltSet.add AbsInterval.MaxInf base in 
  let elts = List.of_seq (AbsIntSet.S.to_seq s) in 
  match elts with
  | a :: b :: _ -> 
    let diff = Z.sub b a in 
    if Z.gt diff Z.zero then 
      let last = AbsIntSet.S.max_elt s in 
      let rec extend i acc = 
        if i > 10 then acc 
        else
          let next = AbsInterval.I (Z.add last (Z.mul diff (Z.of_int i))) in 
          extend (i + 1) (AbsInterval.EltSet.add next acc)
      in
      extend 1 base
    else base
  | _ -> base

(* 변수 하나의 IntSet → 확장된 EltSet 맵 (변수명 포함) *)
let collect_from_mem (mem : AbsMemory2.t) 
    : AbsInterval.EltSet.t AbsInterval.EltSetMap.t =
  if AbsMemory2.is_bot mem then AbsInterval.EltSetMap.empty
  else
    List.fold_left (fun acc (name, absval) ->
      (* name: 변수명, absval: 그 변수의 추상값 *)
      match absval with
      | AbsValue2.AbsInt (AbsIntSet.IntSet s) ->
        let eltset = expand_intset s in
        (* 이미 같은 변수가 있으면 union, 없으면 새로 추가 *)
        let merged = match AbsInterval.EltSetMap.find_opt name acc with
          | Some existing -> AbsInterval.EltSet.union existing eltset
          | None -> eltset
        in
        AbsInterval.EltSetMap.add name merged acc
      | _ -> acc
    ) AbsInterval.EltSetMap.empty (AbsMemory2.bindings mem)

(* States2 전체 순회 → 변수별 EltSet 맵 *)
let build_threshold_map (states : States2.t) 
    : AbsInterval.EltSet.t AbsInterval.EltSetMap.t =
  let result = ref AbsInterval.EltSetMap.empty in
  States2.iter (fun bb ctxt_mem ->
    States2.fold' (fun _ctxt mem () ->
      let mem_map = collect_from_mem mem in
      AbsInterval.EltSetMap.iter (fun name eltset ->
        let key = bb.Basicblock.bb_name ^ "::" ^ name in 
        let merged = match AbsInterval.EltSetMap.find_opt key !result with
          | Some existing -> AbsInterval.EltSet.union existing eltset
          | None -> eltset
        in
        result := AbsInterval.EltSetMap.add key merged !result
      ) mem_map
    ) ctxt_mem ()
  ) states;
!result

let _ =
  let _ = Format.printf "Analyze Start@." in
  (* 공통 *)
  let target_f : Function.t = Module.find Sys.argv.(2) (Init.llmodule ()) in
  let entry = Bbpool.find (target_f.entry) !Bbpool.pool in
  let exit_bb = Bbpool.find (Cfg.exit target_f.cfg) !Bbpool.pool in  
  
  (* Set Analzyer*)
  let init_mem = Analyzer2.init (Init.m ())  in
  let _ = Analyzer2.LoopCounter.set_max_count 5 in
  let init_states = States2.update (entry, MyContext2.empty ()) init_mem States2.empty in
  let _ = Format.printf "set domain analyze...@." in
  let s = Analyzer2.analyze entry init_states in
  let _ = Format.printf "set domain analyze done...@." in
  let s = !Analyzer2.summary in
  let _ = Format.printf "%a\n" States2.pp s in
  let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in
  let b_state = !Analyzer2.summary2 in
  let _ = Format.printf "B STATE %a\n" States2.pp b_state in
  let _ = AbsInterval.global_b := build_threshold_map b_state in
  let _ = Format.printf "\n--------------------\n" in
  let _ = AbsInterval.pp_global_b () in
  let _ = Format.printf "\n--------------------\n" in
  (*let _ = Format.printf "%a\n" States2.pp s2 in *)
  
  let init_mem = Analyzer.init (Init.m ())  in
  let _ = Analyzer.LoopCounter.set_max_count 5 in
  let init_states = States.update (entry, MyContext.empty ()) init_mem States.empty in
  let _ = Format.printf "analyze...@." in

  (* Set arg_esp address as 100000*)
  let _ = TF.tmp_addr := 100000 in
  let s = Analyzer.analyze entry init_states in
  let _ = Format.printf "first analyze done...@." in
  let s1 = !Analyzer.summary in
  let _ = Format.printf "%a\n" States.pp s1 in
  let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in
  
  (* Set arg_esp address as 200000*)
  let _ = Analyzer.LoopCounter.reset () in
  let _ = TF.tmp_addr := 200000 in
  let s = Analyzer.analyze entry init_states in
  let _ = Format.printf "second analyze done...@." in
  let s2 = !Analyzer.summary in
  (* let _ = Format.printf "%a\n" States.pp_exit s2 in
  let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in *)
  (*let _ = Format.printf "%a\n" pp_states s in *)

  (* s1의 exit memory와 s2의 exit memory 를 비교합시다. 다르면? 주소(변수)죠. 같으면? 상수죠 *)
  let s1_exit_mem = States.find_mem (exit_bb, MyContext.empty ()) s1 in
  let s2_exit_mem = States.find_mem (exit_bb, MyContext.empty ()) s2 in 
  let filtered_mem = AbsMemory.filter_mem s1_exit_mem s2_exit_mem in 
  let _ = Format.printf "mem \n %a \n" AbsMemory.pp s1_exit_mem in 
  let _ = Format.printf "filtered mem \n %a \n" AbsMemory.pp filtered_mem in 
  let _ = Format.printf "ENV\n %a \n" Env.pp !Env.env in 
  let reverse_env = Env.reverse_env !Env.env in  

  let var_set = AbsMemory.group_by_value_with_env filtered_mem reverse_env in 
  let arrays = StackShape.detect_arrays target_f var_set s2_exit_mem in 
  let stack = StackShape.build_stack_shape var_set arrays in 
  let _ = StackShape.pp_stack_shape stack var_set in 

  let oc = open_out "output.json" in 
  let fmt = Format.formatter_of_out_channel oc in 
  AbsMemory.pp_grouped_json_with_env fmt filtered_mem reverse_env;
  Format.pp_print_flush fmt ();
  close_out oc;
  Format.printf "JSON written to output.json@.";

  let oc2 = open_out "stack_shape.json" in
  StackShape.pp_json stack oc2;
  close_out oc2;
  Format.printf "stack shape written to stack_shape.json@.";

  ()
