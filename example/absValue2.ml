module F = Format
module AbsAddr = AbsAddr
module AbsInt = AbsInterval

  type elt = IntLiteral of Z.t | AddrLiteral of AbsAddr.elt
  type t = | AbsTop | AbsAddr of  AbsAddr.t | AbsInt of AbsInt.t | AbsBot
  
  let bot = AbsBot
  let top = AbsTop

  let pp fmt v = 
  match v with
  | AbsTop -> F.fprintf fmt "AbsTop"
  | AbsBot -> F.fprintf fmt "AbsBot"
  | AbsInt (v) -> F.fprintf fmt "IntSet : %a" AbsInt.pp v 
  | AbsAddr v -> F.fprintf fmt "AddrSet : %a" AbsAddr.pp v 
  
  let (<=) v1 v2 = 
    match v1, v2 with
    | AbsTop, AbsBot -> false
    | AbsBot, _ -> true
    | _, AbsTop -> true
    | AbsTop, AbsInt n2 -> AbsInt.(AbsInt.top <= n2)
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
      let _ = Format.printf "Test pinpoint@." in 
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
      | AbsAddr n1, AbsAddr n2 -> AbsAddr (AbsAddr.widen n1 n2)
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

