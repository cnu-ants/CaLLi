
let mm = ref None
let llm = ref None
let cg = ref None


let m () = 
  match !mm with
  | None -> failwith "Init.init first"
  | Some m -> m

let llmodule () = 
  match !llm with
  | None -> failwith "Init.init first"
  | Some m -> m

let call_graph () = 
  match !cg with
  | None -> failwith "Init.init first"
  | Some m -> m

let llctx = Llvm.create_context ()

let init () = 
  let _  =Format.printf "init@." in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llmod = Llvm_bitreader.parse_bitcode llctx llmem in
  let ast = Transform.transform_module llmod in  
  let _ = mm := Some ast in
  (* let _ = llm := Some ast.function_map in
  let _ = cg := Some (CallGraph.make_call_graph ast) in  *)
  ast

  (* ast3, ast3.function_map, CallGraph.make_call_graph ast3 *)


let loop_unroll i =
  let _  =Format.printf "unroll@." in
  let unroll = Opt.loop_unroll i Sys.argv.(1) in
  let llmem = Llvm.MemoryBuffer.of_file unroll in
  let llmod = Llvm_bitreader.parse_bitcode llctx llmem in
  let ast = Transform.transform_module llmod in  
  let _ = mm := Some ast in 
  m ()
  (* let _ = llm := Some ast.function_map in
  let _ = cg := Some (CallGraph.make_call_graph ast) in 
  () *)

let transform_call () = 
  let _  =Format.printf "trans call." in
  let _ = mm := Some (Transform2.transform_call (m ())) in 
  m ()

let transform_select () =
  let _  =Format.printf "trans select@." in
  let _ = mm := Some (Transform_select.transform_select (m ())) in 
  m ()

let transform_prune () = 
  let _  =Format.printf "trans prune@." in
  let _ = mm := Some (Transform3.add_prune_node (m ())) in 
  m ()

let make_call_graph () = 
  let _  =Format.printf "make call graph@." in
  let _ = cg := Some (CallGraph.make_call_graph (m ())) in ()

let make_llm () = 
  let _  =Format.printf "make llm@." in
  let _ = llm := Some (m ()).function_map in ()
