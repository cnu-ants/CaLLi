(** Seperate Call instructions into CallSite and ReturnSite instructions *)


let rec pp_term_list ppf term_list = 
  match term_list with
  | _ :: tl -> pp_term_list ppf tl
  | [] -> Format.fprintf ppf "empty"

let rec pp_stmt_list ppf term_list = 
  match term_list with
  | _ :: tl -> pp_stmt_list ppf tl
  | [] -> Format.fprintf ppf "empty"




type st = S of Stmt.t | T of Term.t

let rec transform_call_stmt (stmt_list : Stmt.t list) : st list =
  match stmt_list with
  | hd::tl ->
    (match hd.inst with
    | Call {name; ty; callee; args; _;} ->
      let callsite = Term.CallSite {bb_name=hd.bb_name; callee=callee; args=args} in
      let returnsite : Stmt.t = {bb_name=hd.bb_name; index=hd.index; inst=Inst.ReturnSite {name=name; ty=ty}; loc=hd.loc} in
      [T callsite; S returnsite]@(transform_call_stmt tl)
    | _ -> [S hd]@(transform_call_stmt tl)
    )
  | [] -> []

let split (bb : Basicblock.t) (st_list : st list) : string list = 
  let (name_list, _, _) = 
    List.fold_left
    (fun (name_list, lst, i) st -> 
      match st with
      | S stmt -> (name_list, lst@[stmt], i)
      | T term -> 
        let new_name = if i = 0 then bb.bb_name else bb.bb_name^"#"^(string_of_int i) in
        let new_bb : Basicblock.t = {func_name=bb.func_name; bb_name = new_name; stmts = lst; term = term; loc=bb.loc} in
        let _ = Bbpool.pool := (Bbpool.add new_name new_bb !Bbpool.pool) in
        (name_list@[new_name], [], i+1)
    )
    ([], [], 0)
    st_list
  in
  name_list



let transform_call_pool bb_name : String.t * String.t list =
  let bb = Bbpool.find bb_name !Bbpool.pool in
  let st_list = transform_call_stmt bb.stmts in
  let st_list = st_list@[T bb.term] in

  let new_bb_list = split bb st_list in
  (bb_name, new_bb_list)




let change_next (cfg : Cfg.t) (origin_name : String.t) (new_name : String.t) : Cfg.t =
  let new_cfg = 
    Cfg.fold
    (fun k v (cfg') ->
      let next_list = List.fold_left
        (fun list next-> 
          if next = origin_name then list@[new_name] else list@[next]
        )
        []
        v
      in
      Cfg.add k next_list cfg'
    )
    cfg
    cfg 
  in
  new_cfg


let transform_call_cfg (cfg : Cfg.t) : Cfg.t =
  let new_cfg = 
    Cfg.fold 
    (fun  k v (cfg') -> 
      let (origin_name ,list) = transform_call_pool k in
      let (_, cfg'') = 
        List.fold_right
        (fun bbname (next, cfg'') ->
          ([bbname], (Cfg.add bbname next cfg''))
        )
        list
        (v, cfg') in
      if (List.length list)!= 0 then change_next cfg'' origin_name (List.hd list)
      else
      cfg'
    )
    cfg
    cfg
  in
  new_cfg
  
 

let transform_call_func (func : Function.t) : Function.t =
  let cfg = transform_call_cfg func.cfg in
  let f : Function.t = {function_name=func.function_name; cfg=cfg; params=func.params; metadata=func.metadata; entry=func.entry} in
  f


let transform_call (m : Module.t) : Module.t =
  let function_map = 
    Module.fold
    (fun k v map -> Module.add k (transform_call_func v) map) 
    m.function_map
    Module.M.empty 
  in
  {function_map=function_map; globals=m.globals}
  













