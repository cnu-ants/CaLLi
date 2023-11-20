let calc_name (e : Expr.t) : String.t option = 
  match e with
    | Name {name;_} -> Some name
    | _ -> None

let calc_alias (stmt : Stmt.t) : (string * Metadata.Alias.aliasty) option = 
  match stmt.inst with
  | Load {name; operand; _} -> Some (name, Pointer operand)
  | ICmp {name; cond; operand0; operand1;_} -> Some (name, Predicate {cond=cond; operand0=operand0; operand1=operand1})
  | _ -> None


let make_alias (func : Function.t) = 
  let alias = Cfg.fold
    (fun bb_name _ alias'-> 
      let bb = Bbpool.find bb_name !Bbpool.pool in
      List.fold_left
        (fun alias stmt ->
        match (calc_alias stmt) with
        | Some (key, v) -> Metadata.Alias.add key v alias
        | None -> alias
        )
        alias' bb.stmts
    ) 
    func.cfg
    Metadata.Alias.empty 
  in 
  let metadata : Metadata.t = Meta {alias=alias} in
  metadata
