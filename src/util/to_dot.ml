
module Make (Summary : States.S) = 
  struct

type summaryty = Summary.t

let summary = ref Summary.empty

let bb_to_node (bb : Basicblock.t): Dot.Node_Stmt.t = 
  let body =  List.fold_left 
    (fun str (s : Stmt.t) -> str^(Format.asprintf "%a\\l" Inst.pp s.inst))
    ""
    bb.stmts
  in 
  let loc = if bb.loc = "" then "" else "(loc:%"^bb.loc^")" in
  let body = bb.bb_name^loc^" :\\l"^body in
  let body = body^(Format.asprintf "%a\\l" Term.pp bb.term) in
  let label : Dot.Attr.t =  Label (body) in
  let shape : Dot.Attr.t = Shape (Record) in
  let style : Dot.Attr.t = Style (Filled) in
  let color : Dot.Attr.t = FillColor (Lavenderblush) in
  (* let tooltip : Dot.Attr.t = ToolTip (Format.asprintf "%a" Summary.pp_ctxtMem (Summary.find bb !summary)) in *)
  let tooltip : Dot.Attr.t = ToolTip ("") in

  let node : Dot.Node_Stmt.t = {id=bb.bb_name; attr_list=[ style; color;label; shape; tooltip]} in
  node


let rec cfg_to_dot (cfg : Cfg.t) (bb : Basicblock.t) visit = 
  if List.mem bb visit then visit, []
  else
    let next_lst = Cfg.next bb cfg in
    let visit = bb::visit in 
    (* let _ = 
      Summary.CtxtM.iter
      (fun ctxt _ -> Format.printf "%a\n" FlaCtxt.pp ctxt)
      (Summary.find bb !summary)
    in *)
    let _, visit, lst = 
      List.fold_left 
      (fun (index, visit, lst) (bb':Basicblock.t)-> 
        let edge : Dot.Stmt.t = Edge_Stmt {id=bb.bb_name; edgeRHS=bb'.bb_name; port=index} in 
        let visit', lst' = cfg_to_dot cfg bb' visit in
        index+1, visit', lst@lst'@[edge])
      (0, visit, [Node_Stmt (bb_to_node bb)])
      next_lst in
    visit, lst

let func_to_dot (f : Function.t) : Dot.DIGraph.t = 

  (* let _ = Bbpool.iter (fun k _ -> Format.printf "fname : %s\n" k) !Bbpool.pool in *)
  let oc = open_out (f.function_name^".dot") in
  (* let _ = Format.printf "find : %s\n" (f.function_name^"#entry") in *)
  let _, stmts = cfg_to_dot f.cfg (Bbpool.find_bb f.entry) [] in
  let g : Dot.DIGraph.t = {id=f.function_name; stmt_list=stmts;} in
  let _ = Format.fprintf (Format.formatter_of_out_channel oc) "%a" Dot.DIGraph.pp g in
  let _  = close_out oc in
  g

let make f = 
  func_to_dot f

end
