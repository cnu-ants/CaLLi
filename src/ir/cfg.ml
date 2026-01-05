(**

The CFG module represents the Control Flow Graph of CaLLi IR.

 **)

module M = Map.Make(String)

type t = (String.t list) M.t 

let empty = M.empty
let find = M.find
let add = M.add
let fold = M.fold
let iter = M.iter
let map = M.map
let mem = M.mem

(* The next function returns the successors of a basic block as a list in the CFG.  *)
let next (bb : Basicblock.t) (cfg : t) : Basicblock.t list =
  M.find bb.bb_name cfg |> List.map (fun b -> Bbpool.find b !Bbpool.pool)

(* pretty print CFG *)
let pp ppf m =
  M.iter 
  (fun k v ->
    let _ = Format.fprintf ppf "%s -> " k in
    let _ = List.iter 
    (fun b -> 
        Format.fprintf ppf "%s " b
    ) 
    v
    in
    Format.printf "\n"
  ) 
  m



module StringSet = Set.Make(String)

let entry cfg = 
  let all_succs = fold (fun key _ acc -> 
    key::acc) cfg [] in
  let entrys = fold (fun _ succs acc ->
    List.filter (fun node -> not (List.mem node succs) ) acc
    ) cfg all_succs in
  List.hd entrys


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







