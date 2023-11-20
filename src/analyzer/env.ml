module M = Map.Make(String)
type t =  Val.t M.t

let empty : t = M.empty
let env : t ref = ref M.empty
let find x e = 
    let res_opt = M.find_opt x e in
    match res_opt with
    | None -> Val.Addr ("",0,0) (*failwith (Format.asprintf "Not Found : %s\n" x)*)
    | Some a -> a
  
let add = M.add
let iter = M.iter
let mem = M.mem

let pp ppf m =
iter (fun k v -> Format.fprintf ppf " %s -> %a@\n" k Val.pp v) m    