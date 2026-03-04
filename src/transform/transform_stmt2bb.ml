

let transform_bbpool (bb : Basicblock.t) : Basicblock.t list=
  let new_bbs, _ = List.fold_left
    (fun (new_bbs, i) stmt ->
      let new_bb : Basicblock.t = {bb with bb_name=bb.bb_name^"!"^(string_of_int i); stmts=[stmt]; term=None;} in
      let _ = Bbpool.pool := Bbpool.add new_bb.bb_name new_bb !Bbpool.pool in
      ( new_bb::new_bbs, i+1)
    ) ([], 0) bb.stmts in  
  let new_bbs = 
    match bb.term with
    | Some term -> 
      let new_bb : Basicblock.t = {bb with bb_name=bb.bb_name^"!term"; stmts=[]; term=Some term} in
      let _ = Bbpool.pool := Bbpool.add new_bb.bb_name new_bb !Bbpool.pool in
      new_bb::new_bbs
    | None -> new_bbs
  in
  List.rev new_bbs
  

let change_next cfg origin_name new_name =
  Cfg.fold
    (fun k v acc ->
      let next_list =
        List.map (fun next -> 
        
        if next = origin_name then 
          new_name else next) v
      in
      Cfg.add k next_list acc
    )
    cfg
    cfg

let transform_cfg (cfg : Cfg.t) (func : Function.t) : (Cfg.t * String.t * String.t) = 
  let rec connect (lst : Basicblock.t list) succ (cfg : Cfg.t) : Cfg.t = 
    match lst with 
    | [] -> cfg
    | bb::[] -> Cfg.add bb.bb_name succ cfg
    | bb1::bb2::tl -> connect (bb2::tl) succ (Cfg.add bb1.bb_name [bb2.bb_name] cfg)
  in    
  let new_cfg, entry, exit = 
    Cfg.fold 
    (fun k _ (cfg', entry, exit) ->
      let _ = Format.printf "@@@%s@." k in
      let new_bbs = transform_bbpool (Bbpool.find_bb k) in
      let entry = if k = func.entry then (List.hd new_bbs).bb_name else entry in  
      let exit = if k = func.exit then (List.hd (List.rev new_bbs)).bb_name else exit in  
      let cfg'' = connect new_bbs (Cfg.find k cfg') cfg' in
      let cfg'' = change_next cfg'' k (List.hd new_bbs).bb_name in
      let cfg'' = Cfg.remove k cfg'' in
      (cfg'', entry, exit)
    )
    cfg
    (cfg, "", "")
  in
  (new_cfg, entry, exit)
  


let transform_func (func : Function.t) : Function.t = 
  let _ = Format.printf "ORIG CFG %s: %a@." func.function_name Cfg.pp func.cfg in
  let cfg, entry, exit = transform_cfg func.cfg func in
  let f : Function.t = {function_name=func.function_name; cfg=cfg; params=func.params; metadata=func.metadata; 
    entry=entry; exit=exit;} in
  let _ = Format.printf "CFG %s: %a@." func.function_name Cfg.pp cfg in
  let _ = Format.printf "Entry %s : %s@." func.function_name entry in
  f


let transform_stmt2bb (m : Module.t) : Module.t = 
  let function_map = 
    Module.fold
    (fun k v map -> Module.add k (transform_func v) map) 
    m.function_map
    Module.M.empty 
  in
  {function_map=function_map; globals=m.globals}
  











