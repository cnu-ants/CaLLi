
let transform_cfg (cfg : Cfg.t) : Cfg.t = 
  Cfg.fold 
  (fun bb_name next_bbs cfg -> 
    let bb : Basicblock.t = Bbpool.find_bb bb_name in
    let stmt : Stmt.t option = List.find_opt (fun (stmt:Stmt.t) -> (match stmt.inst with | Select _ -> true | _ -> false)) bb.stmts in
    (match stmt with
    | Some stmt -> 
      (match stmt.inst with
      (Select {name; cond; operand0; operand1; ty}) ->
        let bb_name1 = bb_name^(string_of_int stmt.index)^"select1" in
        let bb_name2 = bb_name^(string_of_int stmt.index)^"select2" in
        let bb_name3 = bb.bb_name^"'" in
        let alloca : Stmt.t = {stmt with inst=(Alloc {name=bb_name^name; ty=(Pointer {ty=ty})})} in
        let br_term : Term.t = CondBr {bb_name=bb_name; cond=cond; 
                              succ0=(Name {ty=Label; name=bb_name1}); 
                              succ1=(Name {ty=Label; name=bb_name2});} in
        let load : Stmt.t = {bb_name=bb_name3; index=(-1); loc=stmt.loc;
                        inst=(Load {name=name; operand=Name {ty=ty; name=bb_name^name}; ty=ty})} in
        let store1 : Stmt.t = {bb_name=bb_name1; index=0; loc=stmt.loc;
                          inst=(Store {operand=operand0; name=bb_name^name; ty=(Pointer {ty=ty})})
                          } in 
        let store2 : Stmt.t = {bb_name=bb_name2; index=0; loc=stmt.loc;
                          inst=(Store {operand=operand1; name=bb_name^name; ty=(Pointer {ty=ty})})
                          } in 
        let new_bb1 : Basicblock.t = 
          {bb with stmts=(List.filter (fun (stmt' : Stmt.t) -> if stmt'.index < stmt.index then true else false) bb.stmts)@[alloca]; 
                   term=br_term;
                   bb_name=bb_name} in
        let new_bb2 : Basicblock.t = 
          {bb with stmts=load::(List.filter (fun (stmt' : Stmt.t) -> if stmt'.index > stmt.index then true else false) bb.stmts);
          bb_name=bb_name3;
          } in
        let select_bb1 : Basicblock.t = {bb with bb_name=bb_name1;
                          stmts=[store1];
                          term=Br {bb_name=bb_name1; succ=(Name {ty=Label; name=bb_name3})}} in
        let select_bb2 : Basicblock.t = {bb with bb_name=bb_name2;
                          stmts=[store2];
                          term=Br {bb_name=bb_name2; succ=(Name {ty=Label; name=bb_name3})}}  in
        let _ = Bbpool.pool := Bbpool.add bb_name new_bb1 !Bbpool.pool in 
        let _ = Bbpool.pool := Bbpool.add bb_name1 select_bb1 !Bbpool.pool in 
        let _ = Bbpool.pool := Bbpool.add bb_name2 select_bb2 !Bbpool.pool in 
        let _ = Bbpool.pool := Bbpool.add bb_name3 new_bb2 !Bbpool.pool in 
        let cfg' = Cfg.add bb_name [bb_name1; bb_name2] cfg in
        let cfg' = Cfg.add bb_name1 [bb_name3] cfg' in
        let cfg' = Cfg.add bb_name2 [bb_name3] cfg' in 
        let cfg' = Cfg.add bb_name3 next_bbs cfg' in
        cfg'
      | _ -> failwith "unreachable")
    | None -> cfg
    )
  )
  cfg
  cfg




let transform_func (func : Function.t) : Function.t =
  let cfg = transform_cfg func.cfg in
  let f : Function.t = {function_name=func.function_name; cfg=cfg; params=func.params; metadata=func.metadata; entry=func.entry} in
  f



let transform_select (m : Module.t) : Module.t =
  let function_map = 
    Module.fold
    (fun k v map -> Module.add k (transform_func v) map) 
    m.function_map
    Module.M.empty 
  in
  {function_map=function_map; globals=m.globals}
