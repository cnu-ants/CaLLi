module M = Map.Make(String)
type t = Basicblock.t M.t
let empty = M.empty
let find = M.find
let add = M.add
let iter = M.iter
let fold = M.fold
let mem = M.mem

let pool = ref empty

let find_bb bb_name : Basicblock.t = find bb_name !pool

let pp ppf m =
  iter (fun k (v: Basicblock.t) -> Format.fprintf ppf "%s -> %s\n" k v.bb_name) m

