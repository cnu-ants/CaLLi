module F = Format
module AbsInt = AbsInterval
module AbsAddr = AbsAddr

  type elt = IntLiteral of Z.t | AddrLiteral of AbsAddr.elt
  type t = | AbsTop | AbsAddr of  AbsAddr.t | AbsInt of AbsInt.t | AbsBot

  let bot = AbsBot
  let top = AbsTop

  let pp fmt v =
  match v with
  | AbsTop -> F.fprintf fmt "AbsTop"
  | AbsBot -> F.fprintf fmt "AbsBot"
  | AbsInt (v) -> F.fprintf fmt "IntInterval : %a" AbsInt.pp v
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

  let is_top v1 =
    match v1 with
    | AbsInt i ->
      begin
        match i with
        | IntBot -> true
        | _ -> false
      end
    | AbsTop | AbsBot -> true
    | _ -> false

  let equal v1 v2 =
    v1 <= v2 && v2 <= v1

  let is_singleton v : bool =
    match v with
    | AbsInt i -> AbsInt.is_singleton i
    | AbsAddr a -> AbsAddr.is_singleton a
    | _ -> false

  let extract_value_string v : string option =
    match v with
    | AbsInt i -> AbsInt.extract_value_string i
    | AbsAddr a -> AbsAddr.extract_value_string a
    | _ -> None

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
        failwith ("sub_sle error"^s)

    let app_sgt v1 v2 =
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_sgt n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ ->
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in
        failwith ("sub_sgt error"^s)

    let app_sge v1 v2 =
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_sge n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ ->
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in
        failwith ("sub_sge error"^s)

    let app_ugt v1 v2 =
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_ugt n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ ->
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in
        failwith ("sub_ugt error"^s)

    let app_uge v1 v2 =
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_uge n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ ->
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in
        failwith ("sub_uge error"^s)

    let app_ult v1 v2 =
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_ult n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ ->
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in
        failwith ("sub_ult error"^s)

    let app_ule v1 v2 =
      let _ = Format.printf "  [AbsValue.app_ule]\n" in
      match v1, v2 with
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.app_ule n1 n2)
      | AbsBot, _ -> AbsBot
      | AbsTop, _ -> AbsTop
      | _ ->
        let s = Format.asprintf "v1 : %a\nv2: %a\n" pp v1 pp v2 in
        failwith ("sub_ule error"^s)

    let alpha_int (n:Z.t) = AbsInt (AbsInt.alpha n)

    let alpha_addr (a : AbsAddr.elt) = AbsAddr (AbsAddr.alpha a)

    let alpha literal str =
      match literal with
      | IntLiteral n -> alpha_int n
      | AddrLiteral a -> alpha_addr a

    let widen key v1 v2 =
      match v1, v2 with
      | _, AbsBot -> v1
      | AbsBot, _ -> v2
      | AbsTop, _ -> AbsTop
      | _, AbsTop -> AbsTop
      | AbsInt n1, AbsInt n2 -> AbsInt (AbsInt.widen key n1 n2)
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
      | And ->
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.logand v1 v2)
        | _ -> failwith "BinOp & Error")
      | Or ->
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.logor v1 v2)
        | _ -> failwith "BinOp | Error")
      | Xor ->
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.BinOp.logxor v1 v2)
        | _ -> failwith "BinOp ^ Error")
      | _ -> AbsBot

    (* i1 결과를 나타내는 상수들. unknown = [0,1] *)
    let abs_true    = AbsInt (AbsInt.alpha Z.one)
    let abs_false   = AbsInt (AbsInt.alpha Z.zero)
    let abs_unknown =
      AbsInt (AbsInt.join (AbsInt.alpha Z.zero) (AbsInt.alpha Z.one))

    (* 두 구간이 부호 반쪽 기준으로 어떤 관계인지.
       u(x) = x (x>=0) | x + 2^32 (x<0) 는 각 반쪽 안에서 순서를 보존하고,
       음수 반쪽 전체가 비음수 반쪽 전체보다 unsigned로 크다. *)
    type half_rel =
      | SameHalf        (* 둘 다 같은 반쪽 -> signed 비교와 결과가 동일 *)
      | UGreater        (* n1의 모든 값 >u n2의 모든 값 *)
      | ULess           (* n1의 모든 값 <u n2의 모든 값 *)
      | UUnknown        (* 한쪽이 부호 경계를 걸침 -> 판정 불가 *)

    let half_rel (n1 : AbsInt.t) (n2 : AbsInt.t) : half_rel =
      let nn1 = AbsInt.all_nonneg n1 and nn2 = AbsInt.all_nonneg n2 in
      let ng1 = AbsInt.all_neg n1    and ng2 = AbsInt.all_neg n2 in
      if (nn1 && nn2) || (ng1 && ng2) then SameHalf
      else if ng1 && nn2 then UGreater
      else if nn1 && ng2 then ULess
      else UUnknown

    let compop (op : Calli.Cond.t) n1 n2 str =
      match op with
      | Eq ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 == v2))
        | _ -> abs_unknown
        )
      | Ne ->
        (match n1, n2 with
        | AbsBot, _ | _, AbsBot -> AbsBot
        | AbsTop, _ | _, AbsTop -> AbsTop
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 != v2))
        | _ -> abs_unknown
        )

      (* ---- signed ---- *)
      | Sgt ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v2 < v1))
        | _ -> abs_unknown
        )
      | Sge ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v2 <= v1))
        | _ -> abs_unknown
        )
      | Slt ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 < v2))
        | _ -> abs_unknown
        )
      | Sle ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 -> AbsInt (AbsInt.CompOp.(v1 <= v2))
        | _ -> abs_unknown
        )

      (* ---- unsigned ---- *)
      | Ugt ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 ->
          (match half_rel v1 v2 with
           | SameHalf -> AbsInt (AbsInt.CompOp.(v2 < v1))
           | UGreater -> abs_true
           | ULess    -> abs_false
           | UUnknown -> abs_unknown)
        | _ -> abs_unknown
        )
      | Uge ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 ->
          (match half_rel v1 v2 with
           | SameHalf -> AbsInt (AbsInt.CompOp.(v2 <= v1))
           | UGreater -> abs_true
           | ULess    -> abs_false
           | UUnknown -> abs_unknown)
        | _ -> abs_unknown
        )
      | Ult ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 ->
          (match half_rel v1 v2 with
           | SameHalf -> AbsInt (AbsInt.CompOp.(v1 < v2))
           | UGreater -> abs_false
           | ULess    -> abs_true
           | UUnknown -> abs_unknown)
        | _ -> abs_unknown
        )
      | Ule ->
        (match n1, n2 with
        | AbsInt v1, AbsInt v2 ->
          (match half_rel v1 v2 with
           | SameHalf -> AbsInt (AbsInt.CompOp.(v1 <= v2))
           | UGreater -> abs_false
           | ULess    -> abs_true
           | UUnknown -> abs_unknown)
        | _ -> abs_unknown
        )