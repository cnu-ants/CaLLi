module F = Format
    type elt = Z.t
    module S = Set.Make(Z)

    type t = 
      | IntTop (* = [MinIntInf, MaxInfInt] *)
      | IntSet of S.t
      | IntBot

    let bot :t = IntBot
    let top :t = IntTop
    
    let to_string x = Z.to_string x

    let is_singleton n : bool =
      match n with 
      | IntSet s -> S.cardinal s = 1
      | _ -> false

    let extract_value_string (s : t) : string option =
      match s with
      | IntSet set when S.cardinal set = 1 ->
          Some (Z.to_string (S.min_elt set))
      | _ -> None

    let fold f s init = 
      match s with
      | IntTop -> S.fold f (S.empty) init 
      | IntBot -> failwith "fold Error6"
      | IntSet s -> S.fold f s init

    let iter f s = 
      match s with
      | IntTop -> failwith "iter Error"
      | IntBot -> failwith "iter Error"
      | IntSet s -> S.iter f s

    let alpha (n:Z.t) = 
      IntSet (S.add n S.empty)
    
    let join n1 n2 = 
      match n1, n2 with
      | IntTop, _ | _, IntTop -> IntTop
      | IntBot, _ -> n2
      | _, IntBot -> n1
      | IntSet s1, IntSet s2 -> IntSet (S.union s1 s2)

    let meet n1 n2 = 
      match n1, n2 with
      | IntBot, _ | _, IntBot -> IntBot
      | IntTop, _ -> n2
      | _, IntTop -> n1
      | IntSet s1, IntSet s2 -> if (S.inter s1 s2) = S.empty then IntBot else IntSet (S.inter s1 s2)

    let sub n1 n2 = 
      match n1, n2 with
      | IntBot, _ -> IntBot
      | _, IntBot -> n1
      | IntTop, _ -> IntTop
      | _, IntTop -> IntBot
      | IntSet s1, IntSet s2 -> 
        let s = S.filter (fun e -> if S.mem e s2 then false else true) s1 in
        if s = S.empty then IntBot else IntSet s
    
    let app_eq n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> 
        let s = S.filter (fun e -> if S.mem e s2 then true else false) s1 in
        if s = S.empty then IntBot else IntSet s
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot
      | _ -> IntTop
      
 
    let app_ne n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> 
        let s = S.filter (fun e -> if S.mem e s2 then false else true) s1 in
        if s = S.empty then IntBot else IntSet s
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot
      | _ -> IntTop
   
    let app_slt n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> 
        let min = S.min_elt s2 in
        let s = S.filter (fun e -> if e < min then true else false) s1 in
        if s = S.empty then IntBot else IntSet s
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot
      | _ -> IntTop
    
    let app_sle n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> 
        let min = S.min_elt s2 in
        let s = S.filter (fun e -> if e <= min then true else false) s1 in
        if s = S.empty then IntBot else IntSet s
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot
      | _ -> IntTop
    
    let app_sge n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> 
        let max = S.max_elt s2 in
        let s = S.filter (fun e -> if e >= max then true else false) s1 in
        if s = S.empty then IntBot else IntSet s
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot
      | _ -> IntTop
    
    let app_sgt n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> 
        let max = S.max_elt s2 in
        let s = S.filter (fun e -> if e > max then true else false) s1 in
        if s = S.empty then IntBot else IntSet s
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot
      | _ -> IntTop

    module CompOp = struct
      let (==) n1 n2 =
        match n1, n2 with
        | IntSet s1, IntSet s2 -> 
          if S.equal s1 s2 then alpha (Z.of_int 1) else
          if (S.inter s1 s2) = S.empty then alpha (Z.of_int 0) else
           join (alpha (Z.of_int 0)) (alpha (Z.of_int 1))
        | _ -> join (alpha (Z.of_int 0)) (alpha (Z.of_int 1))
      
      let (!=) n1 n2 = 
        match n1, n2 with
        | IntSet s1, IntSet s2 -> 
          if S.equal s1 s2 then alpha (Z.of_int 0) else
          if (S.inter s1 s2) = S.empty then alpha (Z.of_int 1) else
           join (alpha (Z.of_int 0)) (alpha (Z.of_int 1))
        | _ -> join (alpha (Z.of_int 0)) (alpha (Z.of_int 1))

      let (<) n1 n2 = 
        match n1, n2 with
        | IntBot, _ | _, IntTop -> IntTop
        | IntTop, _ | _, IntBot -> IntBot
        | IntSet s1, IntSet s2 -> 
            if (S.max_elt s1) < (S.min_elt s2) then alpha (Z.of_int 1) else 
            if (S.max_elt s2) < (S.min_elt s1) then alpha (Z.of_int 0) else 
            join (alpha (Z.of_int 0)) (alpha (Z.of_int 1))

      let (<=) n1 n2 = 
        match n1, n2 with
        | IntBot, _ | _, IntTop -> IntTop
        | IntTop, _ | _, IntBot -> IntBot
        | IntSet s1, IntSet s2 -> 
            if (S.max_elt s1) <= (S.min_elt s2) then alpha (Z.of_int 1) else 
            if (S.max_elt s2) <= (S.min_elt s1) then alpha (Z.of_int 0) else 
            join (alpha (Z.of_int 0)) (alpha (Z.of_int 1))
      
      let (>) n1 n2 = 
        match n1, n2 with
        | IntBot, _ | _, IntTop -> IntTop
        | IntTop, _ | _, IntBot -> IntBot
        | IntSet s1, IntSet s2 -> n2 < n1

      let (>=) n1 n2 = 
        match n1, n2 with
        | IntBot, _ | _, IntTop -> IntTop
        | IntTop, _ | _, IntBot -> IntBot
        | IntSet s1, IntSet s2 -> n2 <= n1

  end

    let pp fmt n =
      match n with
      | IntTop -> F.fprintf fmt "IntTop"
      | IntBot -> F.fprintf fmt "IntBot"
      | IntSet s -> let _ = F.fprintf fmt "{" in
                  let _ = 
                  (F.pp_print_seq ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
                  (fun fmt (v : Z.t) -> 
                    F.fprintf fmt "%s" (Z.to_string v)
                  )
                  fmt
                  (S.to_seq s)) in
                  F.fprintf fmt "}"

    
(** Partial order *)
    let (<=) n1 n2 =
      match n1, n2 with
      | _, IntTop -> true
      | IntBot, _ -> true
      | IntTop, _ | _, IntBot -> false
      | IntSet s1, IntSet s2 -> S.subset s1 s2

    let (=) n1 n2 = 
      match n1, n2 with
      | IntSet s1, IntSet s2 -> S.equal s1 s2
      | _ -> false

    let widen n1 n2 =
      (*let _ = Format.printf "widen %a %a@." pp n1 pp n2 in *)
      let res = if n1 <= n2 then
          if n1 = n2 then n2
          else
            IntTop
        else join n1 n2 in res
      (*in
      let _ = Format.printf "widen res %a@." pp res in res*)


    module BinOp = struct
      let (+) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.(+) v v') s') s2 s) s1 S.empty)

      let (-) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.(-) v v') s') s2 s) s1 S.empty)

      let ( * ) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.( * ) v v') s') s2 s) s1 S.empty)

      let (/) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.(/) v v') s') s2 s) s1 S.empty)
      
      let (%) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.(rem) v v') s') s2 s) s1 S.empty)
      
      let (>>) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.shift_right v (Z.to_int v')) s') s2 s) s1 S.empty)
      
      let (<<) n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.shift_left v (Z.to_int v')) s') s2 s) s1 S.empty)
    
      let logor n1 n2 =
        match n1, n2 with
        | IntBot, _ | _, IntBot -> IntBot
        | IntTop, _ | _, IntTop -> IntTop
        | IntSet s1, IntSet s2 -> 
          IntSet (S.fold (fun v s -> S.fold (fun v' s' -> S.add (Z.(logor) v v') s') s2 s) s1 S.empty)
    end

    let pp_elt fmt elt = 
      F.fprintf fmt "%d" elt

