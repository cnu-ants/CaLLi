module M = Map.Make(String)
type t =  string M.t

let empty : t = M.empty
let env : t ref = ref M.empty
let find x e = 
    let res_opt = M.find_opt x e in
    match res_opt with
    | None -> ""
    | Some a -> a
  
let add = M.add
let iter = M.iter
let mem = M.mem

let pp ppf m =
iter (fun k v -> Format.fprintf ppf " %s -> %s@\n" k v) m   

let reverse_env (e: t) : t = 
  M.fold (fun key value acc ->
  M.add value key acc
) e M.empty
