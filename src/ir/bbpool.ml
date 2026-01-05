module M = Map.Make(String)
type t = Basicblock.t M.t
let empty = M.empty
let find = try M.find with | _ -> failwith "bbpool find err"
let add = M.add
let iter = M.iter
let fold = M.fold
let mem = M.mem

let pool = ref empty

let find_bb bb_name : Basicblock.t = 
  if mem bb_name !pool
    then find bb_name !pool
  else failwith "bbpool not found"

let find_bb_from_pool bb_name bbpool : Basicblock.t = 
  if mem bb_name bbpool
    then find bb_name bbpool
  else failwith "bbpool not found"

let pp ppf m =
  iter (fun k (v: Basicblock.t) -> Format.fprintf ppf "%s -> %s\n" k v.bb_name) m

