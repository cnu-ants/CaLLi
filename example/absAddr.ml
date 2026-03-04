module F = Format

    type elt = string
    (*module S = Set.Make(struct type t = elt let compare = compare end)*)
    module S = Set.Make(String)
    type t = 
      | AddrTop
      | AddrSet of S.t
      | AddrBot
    
    let bot = AddrBot
    let top = AddrTop
    (*
    let (<=) v1 v2 = 
      match v1, v2 with
      | _, AddrTop | AddrBot, _ -> true
      | AddrTop, _ | _, AddrBot -> false
      | AddrSet s1, AddrSet s2 -> (S.subset s1 s2)
      *)

    let (<=) n1 n2 =
      match n1, n2 with
      | AddrTop, AddrTop -> true
      | AddrBot, AddrBot -> true
      | _, AddrTop | AddrBot, _ -> true
      | AddrTop, _ | _, AddrBot -> false
      | AddrSet s1, AddrSet s2 -> S.subset s1 s2
  

    let join v1 v2 = 
      match v1, v2 with
      | AddrTop, _ | _, AddrTop -> AddrTop
      | AddrBot, _ -> v2
      | _, AddrBot -> v1
      | AddrSet s1, AddrSet s2 -> AddrSet (S.union s1 s2)

    let meet v1 v2 = 
      match v1, v2 with
      | AddrBot, _ | _, AddrBot -> AddrBot
      | AddrTop, _ -> v2
      | _, AddrTop -> v1
      | AddrSet s1, AddrSet s2 -> AddrSet (S.inter s1 s2)
    
    let alpha n = 
      AddrSet (S.add n S.empty)

    let fold f s init = 
      match s with
      | AddrTop -> failwith "fold Error1"
      | AddrBot -> failwith "fold Error2"
      | AddrSet s -> S.fold f s init

    let widen n1 n2 = 
        if n1 <= n2 then
          if n1 = n2 then n2
          else AddrTop
        else join n1 n2

    let min_elt s = 
      match s with
      | AddrTop -> failwith "fold Error3"
      | AddrBot -> failwith "fold Error4"
      | AddrSet s -> S.min_elt s
      
    let pp fmt v = 
      match v with
      | AddrTop -> F.fprintf fmt "AddrTop"
      | AddrBot -> F.fprintf fmt "AddrBot"
      | AddrSet s -> let _ = F.fprintf fmt "{ " in 
                      let _ =S.iter 
                    (fun v -> 
                      F.fprintf fmt "%s " v
                    ) s in 
                    F.fprintf fmt " }"

    let is_singleton a : bool =
      match a with 
      | AddrSet s -> S.cardinal s = 1
      | _ -> false 
    
    let extract_value_string a : string option = 
      match a with
      | AddrSet s when not (S.is_empty s) -> 
        Some (S.min_elt s)
      | _ -> None