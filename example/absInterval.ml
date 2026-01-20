module F = Format
type elt = I of Z.t | MinInf | MaxInf
type t = IntInterval of {min:elt; max:elt} | IntBot

let bot :t = IntBot
let top :t = IntInterval {min=MinInf; max=MaxInf}

module Elt = struct

  let zero = 
    I (Z.of_int 0)

  let one = 
    I (Z.of_int 1)

  let minus_one = 
    I (Z.of_int (-1))

  let compare (e1 : elt) (e2 : elt) : int =
  match e1, e2 with
  | MinInf, MinInf -> 0
  | MaxInf, MaxInf -> 0
  | I i1, I i2 -> Z.compare i1 i2
  | MinInf, _ -> -1
  | _, MinInf -> 1
  | MaxInf, _ -> 1
  | _, MaxInf -> -1


  let (<)  a b = compare a b < 0
let (<=) a b = compare a b <= 0
let (>)  a b = compare a b > 0
let (>=) a b = compare a b >= 0
let (==) a b = compare a b = 0

(*
  let (<=) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, _ | _, MaxInf -> true
    | I i1, I i2 -> Z.leq i1 i2
    | _ -> false

  let (==) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MinInf | MaxInf, MaxInf -> true
    | I i1, I i2 -> Z.equal i1 i2
    | _ -> false
  *)
  let (+) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(+)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.add i1 i2)

  let (-) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(-)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.sub i1 i2)

  let ( * ) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(*)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.mul i1 i2)

  let (/) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(/)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.div i1 i2)
  
  let (%) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(%)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.rem i1 i2)
  
  let (&) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(and)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(logand) i1 i2)
  
  let (or) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(or)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(logor) i1 i2)

  let xor (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(xor)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(logxor) i1 i2)

  let (>>) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(>>)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(shift_right) i1 (Z.to_int i2))
  
  let (<<) (e1 : elt) (e2 : elt) =
    match e1, e2 with
    | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(<<)"
    | MinInf, _ | _, MinInf -> MinInf
    | MaxInf, _ | _, MaxInf -> MaxInf
    | I i1, I i2 -> I (Z.(shift_left) i1 (Z.to_int i2))

  let logor (e1 : elt) (e2 : elt) =
  match e1, e2 with
  | MinInf, MaxInf | MaxInf, MinInf -> failwith "Error : Elt.(logor)"
  | MinInf, _ | _, MinInf -> MinInf
  | MaxInf, _ | _, MaxInf -> MaxInf
  | I i1, I i2 -> I (Z.(logor) i1 i2)

  let min_elt (elts : elt list) : elt = 
    List.fold_left
    (fun min e -> if e <= min then e else min)
    (List.hd elts)
    elts

  let max_elt (elts : elt list) : elt = 
    List.fold_left
    (fun max e -> if max <= e then e else max)
    (List.hd elts)
    elts


  let next_pow (e : elt) : elt = 
    let res = ref zero in
    while !res <= e do 
      res := !res * I (Z.of_int 2)
    done;
    !res 

end

let alpha (n: Z.t) : t = 
    IntInterval {min=I n; max=I n}

(** Partial order *)
let (<=) (n1:t) (n2:t) : bool =
  match n1, n2 with
  | IntBot, _ -> true
  | _, IntBot -> false
  | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    if (Elt.(min1 <= min0) && Elt.(max0 <= max1)) then true 
    else false

let join n1 n2 = 
  match n1, n2 with
  | IntBot, _ -> n2
  | _, IntBot -> n1
  | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    IntInterval {min=Elt.min_elt [min0; min1]; max=Elt.max_elt [max0; max1]}

let meet n1 n2 = 
  match n1, n2 with
  | IntBot, _ | _, IntBot -> IntBot
  | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
    IntInterval {min=Elt.max_elt [min0; min1]; max=Elt.min_elt [max0; max1]}


module BinOp = struct

  let (+) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      IntInterval {min=Elt.(min0+min1); max=Elt.(max0+max1)}

  let (-) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      IntInterval {min=Elt.(min0-min1); max=Elt.(max0-max1)} 

  let ( * ) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      let mul = [Elt.(min0*min1); Elt.(min0*max1); 
        Elt.(max0*min1); Elt.(max0*max1)] in
      IntInterval {min=Elt.min_elt mul; max=Elt.max_elt mul}

  let (/) n1 n2 : t =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(min1 == Elt.zero) && Elt.(max1 == Elt.zero)
        then IntInterval {min=MinInf; max=MaxInf}
      else if Elt.(min1 == Elt.zero)
        then IntInterval {min=Elt.(min0 / max1); max=MaxInf}
      else if Elt.(max1 == Elt.zero)
        then IntInterval {min=MinInf; max=Elt.(/) max0 min1}
      else if (alpha (Z.zero)) <= n2 
        then IntInterval {min=MinInf; max=MaxInf}
      else 
        let div = [Elt.(min0/min1); Elt.(min0/max1); 
        Elt.(max0/min1); Elt.(max0/max1)] in
        IntInterval {min=Elt.min_elt div; max=Elt.max_elt div}

  let (%) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(min1 == Elt.zero) && Elt.(max1 == Elt.zero)
        then IntInterval {min=MinInf; max=MaxInf}
      else if Elt.(min1 == Elt.zero)
        then IntInterval {min=Elt.(%) min0 max1; max=MaxInf}
      else if Elt.(max1 == Elt.zero)      
        then IntInterval {min=MinInf; max=Elt.(%) max0 min1}
      else if IntInterval {min=Elt.zero; max=Elt.zero} <= n2 
        then IntInterval {min=MinInf; max=MaxInf}
      else 
        let m = [Elt.(min0%min1); Elt.(min0%max1); 
        Elt.(max0%min1); Elt.(max0%max1)] in
        IntInterval {min=Elt.min_elt m; max=Elt.max_elt m}
  
  let (&) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (alpha (Z.zero)) <= n1 || (alpha (Z.zero)) <= n2
        then alpha (Z.zero)
      else if (alpha (Z.minus_one)) <= n1
        then n2
      else if (alpha (Z.minus_one)) <= n2
        then n1
      else if Elt.(min0==max0) && Elt.(min1==max1)
        then IntInterval {min=Elt.(min0 & min1); max=Elt.(max0 & max1)}
      else if Elt.(Elt.zero < min0) && Elt.(Elt.zero < min1)
        then IntInterval {min=Elt.zero; max=Elt.min_elt [max0; max1]}
      else top


  let (or) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (alpha (Z.minus_one)) <= n1 || (alpha (Z.minus_one)) <= n2
        then alpha (Z.minus_one)
      else if (alpha (Z.zero)) <= n1
        then n2
      else if (alpha (Z.zero)) <= n2
        then n1
      else if Elt.(min0==max0) && Elt.(min1==max1)
        then IntInterval {min=Elt.(or) min0 min1; max=Elt.(or) max0 max1}
      else if Elt.(Elt.zero < min0) && Elt.(Elt.zero < min1) && max0 != MaxInf && max1 != MaxInf
        then IntInterval {min=Elt.zero; max=Elt.(Elt.next_pow(Elt.((Elt.max_elt [max0; max1]) + Elt.one)) - Elt.one)}
      else top

  let xor n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (alpha (Z.zero)) <= n1
        then n2
      else if (alpha (Z.zero)) <= n2
        then n1
      else if Elt.(min0==max0) && Elt.(min1==max1)
        then IntInterval {min=Elt.(xor) min0 min1; max=Elt.(xor) max0 max1}
      else if Elt.(Elt.zero < min0) && Elt.(Elt.zero < min1) && max0 != MaxInf && max1 != MaxInf
        then IntInterval {min=Elt.zero; max=Elt.(Elt.next_pow(Elt.((Elt.max_elt [max0; max1]) + Elt.one)) - Elt.one)}
      else top
  

  let (<<) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(max1 < Elt.zero)
        then IntBot
      else 
        n1 * IntInterval {min=Elt.(Elt.one << min1); max=Elt.(Elt.one << max1)}

  let (>>) n1 n2 = 
    match n1, n2 with    
    | IntBot, _ | _, IntBot -> IntBot
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if Elt.(max1 < Elt.zero)
        then IntBot
      else 
        let m = [Elt.(min0 >> min1); Elt.(min0 >> max1); 
        Elt.(max0 >> min1); Elt.(max0 >> max1)] in
        IntInterval {min=Elt.min_elt m; max=Elt.max_elt m}

  let logor n1 n2 =
  match n1, n2 with
  | IntBot, _ | _, IntBot -> IntBot
  | IntInterval { min = min0; max = max0 }, IntInterval { min = min1; max = max1 } ->
      if Elt.(min0 == max0) && Elt.(min1 == max1) then
        let v = Elt.(logor) min0 min1 in
        IntInterval { min = v; max = v }
      else (* Safe upper bound: OR of two positive bounded intervals is in [0, 2^k - 1],
           where k = ceil_log2(max(max0, max1) + 1). *)
        let hi = Elt.max_elt [ max0; max1 ] in
        let ub = Elt.(Elt.next_pow(Elt.(hi + Elt.one)) - Elt.one) in
        IntInterval { min = Elt.zero; max = ub }


end

module CompOp = struct
  let (==) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> alpha (Z.zero)
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
      if (Elt.(min0 == max0) && Elt.(min1 == max1) && Elt.(min0 == min1)) 
        then alpha (Z.one)
      else if Elt.(min1 > max0) || Elt.(min0 > max1) 
        then alpha (Z.zero)
      else IntInterval {min=Elt.zero; max=Elt.one}

  let (!=) n1 n2 = 
    match (n1 == n2) with
    | IntInterval {min; max} ->
      if Elt.(==) min Elt.zero
        then alpha (Z.one)
      else alpha (Z.zero)
    | _ -> failwith "unreachable"

  let (<=) n1 n2 =
    match n1, n2 with
    | IntBot, _ | _, IntBot -> alpha (Z.zero)
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (Elt.(min0 == max0) && Elt.(min1 == max1) && Elt.(min0 == min1)) 
          then alpha (Z.one)
        else if Elt.(min1 > max0) || Elt.(min0 > max1) 
          then alpha (Z.zero)
        else IntInterval {min=Elt.zero; max=Elt.one}

  let (<) n1 n2 = 
    match n1, n2 with
    | IntBot, _ | _, IntBot -> alpha (Z.zero)
    | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        if (Elt.(min0 == max0) && Elt.(min1 == max1) && Elt.(min0 == min1)) 
          then alpha (Z.zero)
        else if Elt.(min1 > max0) || Elt.(min0 > max1) 
          then alpha (Z.one)
        else IntInterval {min=Elt.zero; max=Elt.one}

end


    let sub n1 n2 = 
      match n1, n2 with
      | IntBot, _ -> IntBot
      | _, IntBot -> n1
      | IntInterval {min=min0; max=max0}, IntInterval {min=min1; max=max1} ->
        (*TODO*)
        n1


      
let elt_succ x = Elt.(x + one)
let elt_pred x = Elt.(x - one)

let mk_interval min max =
  if Elt.(min > max) then IntBot else IntInterval { min; max }
   
 
    let app_eq n1 n2 = 
      match n1, n2 with
      | IntInterval { min = min0; max = max0 }, IntInterval { min = min1; max = max1 } ->
      (* Disjoint: no possible equality *)
      if Elt.(min1 > max0) || Elt.(min0 > max1) then
        IntBot
      else
        let min' = if Elt.(min0 >= min1) then min0 else min1 in
        let max' = if Elt.(max0 <= max1) then max0 else max1 in
        IntInterval { min = min'; max = max' }
      | IntBot, _ -> IntBot
      | _, IntBot -> IntBot

let app_ne n1 n2 =
  match n1, n2 with
  | IntBot, _ -> IntBot
  | _, IntBot -> IntBot
  | IntInterval { min = min0; max = max0 }, IntInterval { min = min1; max = max1 } ->
      (* If disjoint, all values in n1 are != all values in n2. *)
      if Elt.(max0 < min1) || Elt.(max1 < min0) then
        n1
      else
        (* Overlap exists. We can only refine when the remaining set is representable as one interval. *)
        if Elt.(min0 >= min1) && Elt.(max0 <= max1) then
          (* n1 subset of n2: no value in n1 can be != all values in n2 *)
          IntBot
        else if Elt.(min0 < min1) && Elt.(max0 <= max1) then
          (* Only left part remains: [min0, min1-1] *)
          mk_interval min0 (elt_pred min1)
        else if Elt.(min0 >= min1) && Elt.(max0 > max1) then
          (* Only right part remains: [max1+1, max0] *)
          mk_interval (elt_succ max1) max0
        else
          (* Two-sided remainder would be a union of intervals; not representable => keep n1. *)
          n1

let app_slt n1 n2 =
  match n1, n2 with
  | IntBot, _ -> IntBot
  | _, IntBot -> IntBot
  | IntInterval { min = min0; max = max0 }, IntInterval { min = min1; max = _ } ->
      (* Require x < y for all y in n2 => x <= min(n2)-1 *)
      mk_interval min0 (if Elt.(max0 <= elt_pred min1) then max0 else elt_pred min1)

let app_sle n1 n2 =
  match n1, n2 with
  | IntBot, _ -> IntBot
  | _, IntBot -> IntBot
  | IntInterval { min = min0; max = max0 }, IntInterval { min = min1; max = _ } ->
      (* Require x <= y for all y in n2 => x <= min(n2) *)
      mk_interval min0 (if Elt.(max0 <= min1) then max0 else min1)

let app_sge n1 n2 =
  match n1, n2 with
  | IntBot, _ -> IntBot
  | _, IntBot -> IntBot
  | IntInterval { min = min0; max = max0 }, IntInterval { min = _; max = max1 } ->
      (* Require x >= y for all y in n2 => x >= max(n2) *)
      mk_interval (if Elt.(min0 >= max1) then min0 else max1) max0

let app_sgt n1 n2 =
  let _ = Format.printf "Test pinpoint2@." in
  let res = match n1, n2 with
  | IntBot, _ -> IntBot
  | _, IntBot -> IntBot
  | IntInterval { min = min0; max = max0 }, IntInterval { min = _; max = max1 } ->
      (* Require x > y for all y in n2 => x >= max(n2)+1 *)
      mk_interval (if Elt.(min0 >= max1) then min0 else max1) max0
  in 
  let _ = Format.printf "Test pinpoin3@." in
  res

let widen n1 n2 =
  match n1, n2 with
  | IntBot, x -> x
  | x, IntBot -> x
  | IntInterval { min = min1; max = max1 }, IntInterval { min = min2; max = max2 } ->
      let min' = if Elt.(min2 < min1) then MinInf else min1 in
      let max' = if Elt.(max2 > max1) then MaxInf else max1 in
      IntInterval { min = min'; max = max' }
  

let fold (f : elt -> 'a -> 'a) (itv : t) (init : 'a) : 'a =
  match itv with
  | IntBot -> init
  | IntInterval { min; max } ->
      begin match min, max with
      | MinInf, _ | _, MaxInf ->
          failwith "fold: infinite interval"
      | I lo, I hi ->
          let rec loop i acc =
            if Z.gt i hi then acc
            else loop Z.(i + one) (f (I i) acc)
          in
          loop lo init
      | _ ->
          failwith "fold: invalid bounds"
      end

let to_string elt = 
    match elt with
    | I i -> F.asprintf "%s" (Z.to_string i)
    | MinInf -> "-inf"
    | MaxInf -> "+inf"
   

let pp_elt _ elt = 
    match elt with
    | I i -> F.printf "%s" (Z.to_string i)
    | MinInf -> F.printf "-inf"
    | MaxInf -> F.printf "+inf"

let pp fmt n =
    match n with
    | IntBot -> F.fprintf fmt "IntBot"
    | IntInterval {min; max} -> F.fprintf fmt "[%a, %a]" pp_elt min pp_elt max
