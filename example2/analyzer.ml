module F = Format
open Calli

let has_flag (s : string) : bool =
  Array.exists (fun x -> String.equal x s) Sys.argv

let get_arg_value (key : string) : string option =
  let rec go i =
    if i + 1 >= Array.length Sys.argv then None
    else if String.equal Sys.argv.(i) key then Some Sys.argv.(i + 1)
    else go (i + 1)
  in
  go 0

let get_int_arg (key : string) ~(default : int) : int =
  match get_arg_value key with
  | None -> default
  | Some s -> (try int_of_string s with _ -> default)

let () =
  let _ = Init.init () in
  let _ = Init.transform_call () in
  let _ = Init.transform_select () in
  let _ = Init.transform_prune () in
  let _ = Init.make_llm () in
  let _ = Init.make_call_graph () in
  let _ = Format.printf "Transform Done@." in
  ()

module Ctxt =
  CallSiteContext.Make
    (struct
      type t = string
      type memty = AbsMemory.t
      let size = ref 1
      let llmodule = Init.llmodule ()
      let call_graph = Init.call_graph ()
      let pp fmt (s : t) = F.fprintf fmt "%s" s
    end)

module States = States.Make (Ctxt) (AbsMemory)

let () =
  let web_enabled = has_flag "--web" in
  let port = get_int_arg "--port" ~default:8080 in
  let interface = match get_arg_value "--iface" with Some s -> s | None -> "0.0.0.0" in

  let target =
    match get_arg_value "--target" with
    | Some f -> f
    | None -> failwith "missing --target <function>"
  in

  let llm = Init.m () in
  let target_f : Function.t = Module.find target (Init.llmodule ()) in
  let entry = Bbpool.find target_f.entry !Bbpool.pool in

  if web_enabled then (
    let module Analyzer = LlvmWebAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF) in
    let module Web = Monitor_server.Make (Analyzer) in

    Analyzer.LoopCounter.set_max_count 30;

    let init_mem = Analyzer.init llm in
    let init_states = States.update (entry, Ctxt.empty ()) init_mem States.empty in
    let rt = Analyzer.init_runtime ~entry ~init_states in

    Web.start ~runtime:rt ~interface ~port
  ) else (
    let module Analyzer = LlvmAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF) in

    Analyzer.LoopCounter.set_max_count 30;

    let init_mem = Analyzer.init llm in
    let init_states = States.update (entry, Ctxt.empty ()) init_mem States.empty in
    let _ = Analyzer.analyze_full entry init_states in

    let s = !Analyzer.summary in
    let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in
    let _ = Format.printf "STATES \n %a\n" States.pp s in
    ()
  )
