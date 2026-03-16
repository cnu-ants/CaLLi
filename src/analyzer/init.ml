let mm = ref None
let llm = ref None
let cg = ref None

let m () =
  match !mm with
  | None -> failwith "Init.init first1"
  | Some m -> m

let llmodule () =
  match !llm with
  | None -> failwith "Init.init first2"
  | Some m -> m

let call_graph () =
  match !cg with
  | None -> failwith "Init.init first3"
  | Some m -> m

let llctx = Llvm.create_context ()

let load_module_from_file path =
  let llmem = Llvm.MemoryBuffer.of_file path in
  let llmod = Llvm_bitreader.parse_bitcode llctx llmem in
  Transform.transform_module llmod

let init path =
  let md = load_module_from_file path in
  let _ = mm := Some md in
  md

let loop_unroll i path =
  let unroll = Opt.loop_unroll i path in
  let md = load_module_from_file unroll in
  let _ = mm := Some md in
  m ()

let save_cache path =
  Cache.save path (m ()) !Bbpool.pool

let load_cache path =
  let md, bbpool = Cache.load path in
  let _ = mm := Some md in
  let _ = Bbpool.pool := bbpool in
  let _ = llm := None in
  let _ = cg := None in
  m ()

let cache_exists path =
  Cache.exists path

let transform_call () =
  let _ = mm := Some (Transform2.transform_call (m ())) in
  m ()

let transform_select () =
  let _ = mm := Some (Transform_select.transform_select (m ())) in
  m ()

let transform_prune () =
  let _ = mm := Some (Transform3.add_prune_node (m ())) in
  m ()

let transform_stmt2bb () =
  let _ = mm := Some (Transform_stmt2bb.transform_stmt2bb (m ())) in
  m ()

let make_call_graph () =
  let _ = cg := Some (CallGraph.make_call_graph (m ())) in
  ()

let make_llm () =
  let _ = llm := Some (m ()).function_map in
  ()
