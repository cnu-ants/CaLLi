
(**

the Basicblock module represents a CaLLi IR corresponding to the basicblock 
in LLVM IR
 - func_name : the name of the function to which the basicblock belongs.
 - loc : The name of the basic block in LLVM IR. 

 **)
type t = {func_name:string; bb_name : string; stmts : Stmt.t list; term : Term.t option; loc : string}
  
let pp ppf (bb : t) =
  let _ = Format.fprintf ppf "<%s> (loc:%s)\n" bb.bb_name bb.loc in
  let _ = 
    List.iter
    (fun (stmt : Stmt.t) -> Format.fprintf ppf "%a\n" Inst.pp stmt.inst)
    bb.stmts
  in
  match bb.term with
  | Some term -> Format.fprintf ppf "%a" Term.pp term
  | None -> ()

