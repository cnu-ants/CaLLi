
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

let init () = 
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llmod = Llvm_bitreader.parse_bitcode llctx llmem in
  let ast = Transform.transform_module llmod in  
  let _ = mm := Some ast in
  (*let _ = llm := Some ast.function_map in
  let _ = cg := Some (CallGraph.make_call_graph ast) in *)  
  ast

  (* ast3, ast3.function_map, CallGraph.make_call_graph ast3 *)


let loop_unroll i =
  let unroll = Opt.loop_unroll i Sys.argv.(1) in
  let llmem = Llvm.MemoryBuffer.of_file unroll in
  let llmod = Llvm_bitreader.parse_bitcode llctx llmem in
  let ast = Transform.transform_module llmod in  
  let _ = mm := Some ast in m ()
  (* let _ = llm := Some ast.function_map in
  let _ = cg := Some (CallGraph.make_call_graph ast) in 
  () *)

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
  let _ = llm := Some (m ()).function_map in ()
