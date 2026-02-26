type t = {function_name : string; cfg : Cfg.t; params : Expr.t list; metadata : Metadata.t; entry : string}

(*
module Visit = Set.Make(String)
let visit = ref Visit.empty

let iter_from_entry f entry cfg = 
let rec iter_from_entry' f (bb:Basicblock.t) cfg = 
   if Visit.mem bb.bb_name !visit then () 
   else
   let _ = f bb.bb_name in
   let succs = next bb cfg in
   let _ = visit := Visit.add bb.bb_name !visit in
   List.iter (fun (succ:Basicblock.t) -> 
     iter_from_entry' f succ cfg) succs
in
let _ = iter_from_entry' f (Bbpool.find entry !Bbpool.pool) cfg in
let _ = visit := Visit.empty in ()


module Visit' = Set.Make(String)
let visit' = ref Visit'.empty

let iter_cfg f entry cfg = 
let rec iter_from_entry' f (bb:string) cfg = 
   if Visit'.mem bb !visit' then () 
   else
   let _ = visit' := Visit'.add bb !visit' in
   let _ = f bb in
   let succs = find bb cfg in
   List.iter (fun (succ:string) -> 
     iter_from_entry' f succ cfg) succs
in
let _ = iter_from_entry' f entry cfg in
let _ = visit' := Visit'.empty in ()

let iter func (f:t) = 
  Cfg.iter_from_entry func f.entry f.cfg *)
