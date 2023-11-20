open Calli

let m, llmodule , call_graph = 
  let llctx = Llvm.create_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  let ast = Transform.transform_module llm in  
  let ast2 = Transform2.transform_call ast in  
  let ast3 = Transform_select.transform_select ast2 in    
  let ast4 = Transform3.add_prune_node ast3 in    
  ast4, ast4.function_map, CallGraph.make_call_graph ast4