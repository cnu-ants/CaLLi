module F = Format

module AbsAddr = 
  struct
    type elt = string
    module S = Set.Make(struct type t = elt let compare = compare end)
    type t = 
      | AddrTop
      | AddrSet of S.t
      | AddrBot
    
    let bot = AddrBot
    let top = AddrTop

    let (<=) v1 v2 = 
      match v1, v2 with
      | _, AddrTop | AddrBot, _ -> true
      | AddrTop, _ | _, AddrBot -> false
      | AddrSet s1, AddrSet s2 -> (S.subset s1 s2)

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
      | AddrTop -> failwith "fold Error"
      | AddrBot -> failwith "fold Error"
      | AddrSet s -> S.fold f s init

      let min_elt s = 
      match s with
      | AddrTop -> failwith "fold Error"
      | AddrBot -> failwith "fold Error"
      | AddrSet s -> S.min_elt s
    let pp fmt v = 
      match v with
      | AddrTop -> F.fprintf fmt "AddrTop"
      | AddrBot -> F.fprintf fmt "AddrBot"
      | AddrSet s -> S.iter 
                    (fun v -> 
                      F.fprintf fmt "{%s }" v
                    ) s

  end  

module AbsInt =
  struct 
    type elt = Z.t
    module S = Set.Make(Z)

    type t = 
      | IntTop (* = [MinIntInf, MaxInfInt] *)
      | IntSet of S.t
      | IntBot

    let bot :t = IntBot
    let top :t = IntTop

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


    
(** Partial order *)
    let (<=) n1 n2 =
      match n1, n2 with
      | IntTop, IntTop -> true
      | IntBot, IntBot -> true
      | _, IntTop | IntBot, _ -> true
      | IntTop, _ | _, IntBot -> false
      | IntSet s1, IntSet s2 -> S.subset s1 s2

    let widen n1 n2 = 
        if n1 <= n2 then
          if n1 = n2 then n2
          else IntTop
        else join n1 n2

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
(* 
      | IntSet s -> let _ = F.fprintf fmt "{" in
                    let _ = S.iter 
                    (fun v -> 
                      F.fprintf fmt "%d, " v
                    ) s in
                    F.fprintf fmt "}" *)

  end

  type elt = IntLiteral of Z.t | AddrLiteral of AbsAddr.elt
  type t = | AbsTop | AbsAddr of  AbsAddr.t | AbsInt of AbsInt.t | AbsBot
  
  let bot = AbsBot
  let top = AbsTop

  let pp fmt v = 
  match v with
  | AbsTop -> F.fprintf fmt "AbsTop"
  | AbsBot -> F.fprintf fmt "AbsBot"
  | AbsInt (v) -> F.fprintf fmt "IntSet : %a" AbsInt.pp v 
  | AbsAddr v -> F.fprintf fmt "%a" AbsAddr.pp v 
  
  let (<=) v1 v2 = 
    match v1, v2 with
    | AbsTop, AbsBot -> false
    | AbsBot, _ -> true
    | _, AbsTop -> true
    | AbsTop, AbsInt n2 -> AbsInt.(AbsInt.IntTop <= n2)
    | AbsInt n1 , AbsBot -> AbsInt.(n1 <= AbsInt.IntBot)
    | AbsAddr a1 , AbsAddr a2 -> AbsAddr.(a1 <= a2)
    | AbsInt n1, AbsInt n2 -> AbsInt.(n1 <= n2) 
    | _ -> false

  let join v1 v2 = 
    (* let _ = Format.printf "v1 : %a\n v2: %a\n" pp v1 pp v2 in *)
    match v1, v2 with
    | AbsTop, _ | _, AbsTop -> AbsTop
    | AbsBot, _ -> v2
    | _, AbsBot -> v1
    | AbsAddr a1 , AbsAddr a2 -> AbsAddr (AbsAddr.join a1 a2)
    | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.join n1 n2)
    | _ -> 
      let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
      failwith ("join error"^s)

    let meet v1 v2 = 
      match v1, v2 with
      | AbsBot, _ | _, AbsBot -> AbsBot
      | AbsTop, _ -> v2
      | _, AbsTop -> v1
      | AbsAddr a1 , AbsAddr a2 -> AbsAddr (AbsAddr.meet a1 a2)
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.meet n1 n2)
      | _ -> failwith "meet error"

    let sub v1 v2 = 
      match v1, v2 with
      | AbsTop, _ -> v2
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.sub n1 n2)
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub error"^s)
    
    let app_eq v1 v2 =
      match v1, v2 with
      | AbsTop, _ -> AbsTop
      | _, AbsTop -> v1
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_eq n1 n2)
      | AbsBot, _ -> AbsBot
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub_eq error"^s)

    let app_ne v1 v2 = 
      match v1, v2 with
      | AbsTop, _ -> AbsTop
      | _, AbsTop -> v1
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_ne n1 n2)
      | AbsBot, _ -> AbsBot
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub_ne error"^s)

    let app_slt v1 v2 = 
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_slt n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub_slt error"^s)
    
    let app_sle v1 v2 = 
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_sle n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub_slt error"^s)
    
    let app_sgt v1 v2 = 
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_sgt n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub_sge error"^s)
    
    let app_sge v1 v2 = 
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_sge n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ -> 
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in 
        failwith ("sub_sge error"^s)
    
    let alpha_int (n:Z.t) = AbsInt (AbsInt.alpha n)

    let alpha_addr (a : AbsAddr.elt) = AbsAddr (AbsAddr.alpha a)

    let alpha literal str = 
      match literal with
      | IntLiteral n -> alpha_int n
      | AddrLiteral a -> alpha_addr a

    let widen v1 v2 = 
      match v1, v2 with
      | _, AbsBot -> v1
      | AbsBot, _ -> v2
      | AbsTop, _ -> AbsTop
      | _, AbsTop -> AbsTop
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.widen n1 n2)
      | AbsAddr n1, AbsAddr n2 -> v1
      | _ -> failwith "widen error"

(*
    let widen v1 v2 = 
      if (v1 <= v2) && v1 <> v2
        then 
          join v1 v2
        else v1
*)

    let binop (op : Calli.Op.t) n1 n2 string =
      match op with   
      | Add -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 + v2))
        | _ -> let _ = Format.printf "Error : %a + %a\n" pp n1 pp n2 in failwith "BinOp + Error")
      | Sub -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 - v2))
        | _ -> failwith "BinOp - Error")
      | Mul -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 * v2))
        | _ -> let _ = Format.printf "Error : %a * %a\n" pp n1 pp n2 in failwith "BinOp * Error")
      | UDiv
      | SDiv -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 / v2))
        | _ -> failwith "BinOp / Error")
      | URem
      | SRem -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 % v2))
        | _ -> failwith "BinOp % Error")
      | AShr -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 >> v2))
        | _ -> failwith "BinOp >> Error")
      | Shl -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.(v1 << v2))
        | _ -> failwith "BinOp << Error")
      | Or -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.logor v1 v2)
        | _ -> failwith "BinOp >> Error")
      
      | _ -> AbsBot
        
    let compop (op : Calli.Cond.t) n1 n2 str = 
      match op with
      | Eq ->
        (match n1, n2 with
        (*| AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        *)
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 == v2))
        | _ -> AbsInt (AbsInt.join (AbsInt.alpha (Z.of_int 0)) (AbsInt.alpha (Z.of_int 1)))
        )
      | Ne -> 
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 != v2))
        | _ -> AbsInt (AbsInt.join (AbsInt.alpha (Z.of_int 0)) (AbsInt.alpha (Z.of_int 1)))
        )
      | Ugt 
      | Sgt ->
        (match n1, n2 with
        (*| AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        *)
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v2 < v1))
        | _ -> AbsInt (AbsInt.join (AbsInt.alpha (Z.of_int 0)) (AbsInt.alpha (Z.of_int 1)))
        )
      | Uge 
      | Sge ->
        (match n1, n2 with
        (*
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        *)
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v2 <= v1))
        | _ -> AbsInt (AbsInt.join (AbsInt.alpha (Z.of_int 0)) (AbsInt.alpha (Z.of_int 1)))
        )
      | Ult 
      | Slt ->
        (match n1, n2 with
        (*
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        *)
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 < v2))
        | _ -> AbsInt (AbsInt.join (AbsInt.alpha (Z.of_int 0)) (AbsInt.alpha (Z.of_int 1)))
        )
      | Ule
      | Sle ->
        (match n1, n2 with
        (*
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        *)
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 <= v2))
        | _ -> AbsInt (AbsInt.join (AbsInt.alpha (Z.of_int 0)) (AbsInt.alpha (Z.of_int 1)))
        )

