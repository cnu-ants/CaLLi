
type t = {function_name : string; cfg : Cfg.t; params : Expr.t list; metadata : Metadata.t; entry : string}

let iter func (f:t) = 
  Cfg.iter_from_entry func f.entry f.cfg

