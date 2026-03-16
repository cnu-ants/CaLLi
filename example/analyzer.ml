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

let get_env_opt (key : string) : string option =
  try Some (Sys.getenv key) with Not_found -> None

let is_option (s : string) : bool =
  String.length s > 0 && Char.equal s.[0] '-'

let get_positional_input () : string option =
  let takes_value =
    [
      "--input";
      "--calli-home";
      "--port";
      "--iface";
      "--target";
      "--cache";
      "--load-cache";
      "--save-cache";
    ]
  in
  let rec go i =
    if i >= Array.length Sys.argv then None
    else
      let cur = Sys.argv.(i) in
      if List.mem cur takes_value then go (i + 2)
      else if is_option cur then go (i + 1)
      else Some cur
  in
  go 1

let get_input_path () : string =
  match get_arg_value "--input" with
  | Some s -> s
  | None -> (
      match get_positional_input () with
      | Some s -> s
      | None ->
          failwith
            "missing input bitcode file: use analyzer.bc --target <function> or --input <file.bc>"
    )

let get_calli_home () : string =
  match get_arg_value "--calli-home" with
  | Some s -> s
  | None -> (
      match get_env_opt "CALLI_HOME" with
      | Some s -> s
      | None ->
          failwith
            "missing --calli-home <CaLLi root> or environment variable CALLI_HOME"
    )

let get_load_cache_path () : string option =
  match get_arg_value "--load-cache" with
  | Some s -> Some s
  | None -> (
      match get_arg_value "--cache" with
      | Some s when Init.cache_exists s -> Some s
      | _ -> None
    )

let get_save_cache_path () : string option =
  match get_arg_value "--save-cache" with
  | Some s -> Some s
  | None -> get_arg_value "--cache"

let () =
  let load_cache_path = get_load_cache_path () in
  let save_cache_path = get_save_cache_path () in

  let loaded =
    match load_cache_path with
    | Some path ->
        let _ = Init.load_cache path in
        let _ = Format.printf "Loaded cache: %s@." path in
        true
    | None -> false
  in

  if not loaded then (
    let input_path = get_input_path () in
    let _ = Init.init input_path in
    let _ = Init.transform_call () in
    let _ = Init.transform_select () in
    let _ = Init.transform_prune () in
    let _ =
      match save_cache_path with
      | Some path ->
          let _ = Init.save_cache path in
          Format.printf "Saved cache: %s@." path
      | None -> ()
    in
    let _ = Format.printf "Transform Done@." in
    ()
  );

  let _ = Init.make_llm () in
  let _ = Init.make_call_graph () in
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
  let interface =
    match get_arg_value "--iface" with
    | Some s -> s
    | None -> "0.0.0.0"
  in

  let target =
    match get_arg_value "--target" with
    | Some f -> f
    | None -> failwith "missing --target <function>"
  in

  let llm = Init.m () in
  let target_f : Function.t = Module.find target (Init.llmodule ()) in
  let entry = Bbpool.find target_f.entry !Bbpool.pool in

  if web_enabled then (
    let calli_home = get_calli_home () in

    let module Analyzer =
      LlvmWebAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF)
    in
    let module Web = Monitor_web.Make (Analyzer) in

    Analyzer.LoopCounter.set_max_count 30;

    let init_mem = Analyzer.init llm in
    let init_states = States.update (entry, Ctxt.empty ()) init_mem States.empty in
    let rt = Analyzer.init_runtime ~entry ~init_states in

    let frontend =
      if has_flag "--no-frontend" then Web.Disabled
      else if has_flag "--frontend-dev" then Web.Dev
      else Web.Static
    in

    Web.start ~frontend ~calli_home ~runtime:rt ~interface ~port ()
  ) else (
    let module Analyzer =
      LlvmAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF)
    in

    Analyzer.LoopCounter.set_max_count 30;

    let init_mem = Analyzer.init llm in
    let init_states = States.update (entry, Ctxt.empty ()) init_mem States.empty in
    let _ = Analyzer.analyze_full entry init_states in

    let s = !Analyzer.summary in
    let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in
    let _ = Format.printf "STATES \n %a\n" States.pp s in
    ()
  )
