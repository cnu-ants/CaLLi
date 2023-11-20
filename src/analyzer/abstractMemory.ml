module F = Format


(* Interface for Abstract Memory required in LlvmAnalyzer*)
module type S =
  sig 
    (* type key *)
    type t
    type valty

    val empty : t
    val bot : t
    val find_opt : string * int * int -> t -> valty option
    val find : string * int * int -> t -> valty
    val update : string * int * int -> valty -> t -> t
    val (<=) : t -> t -> bool
    val join : t -> t -> t
    val meet : t -> t -> t
    val widen : t -> t -> t
    val pp : Format.formatter -> t -> unit
    
  end



(* 
  Functor building an implementation of the abstract memory 
given abstract value type.

  The abstract memory is structured as a map, where the key is an address 
and the value is an abstract value. The address is a tuple consisting of the 
basicblock name, the index of the instruction, and an random number for 
uniqueness in memory addresses.

 *)
module Make(AbsVal : AbstractValue.S) : (S with type valty = AbsVal.t) = 
  struct
    
    type key = string * int * int
    module M = Map.Make 
    (struct 
      type t = key 
      let compare = compare 
    end)
    
    type t = MemBot | Mem of AbsVal.t M.t
    type valty = AbsVal.t

    let empty = Mem (M.empty)
    let bot = MemBot

    let find_opt x mem = 
      match mem with
      | Mem mem -> M.find_opt x mem
      | MemBot -> None

    let find x mem = 
      match mem with
      | Mem mem ->
        (try M.find x mem
        with _ -> AbsVal.bot)
      | MemBot -> AbsVal.bot

    let rec update x (v: AbsVal.t) mem = 
      match mem with 
      | Mem mem -> Mem (M.add x v mem)
      | MemBot -> update x v empty

    let (<=) mem1 mem2 = 
      match mem1, mem2 with
      | MemBot, _ -> true
      | Mem mem1, Mem mem2 -> 
        M.for_all (fun x v -> M.mem x mem2 && AbsVal.(v <= (M.find x mem2))) mem1
      | _, MemBot -> false

    let join mem1 mem2 =
      match mem1, mem2 with
      | Mem mem1, Mem mem2 ->
        Mem (M.union (fun _ v1 v2 -> Some AbsVal.(join v1 v2)) mem1 mem2)
      | MemBot, _ -> mem2
      | _, MemBot -> mem1

    let widen mem1 mem2 = 
      match mem1, mem2 with
      | Mem mem1, Mem mem2 ->
        Mem (M.union 
        (fun _ v1 v2 -> 
          Some (AbsVal.widen v1 v2)
        ) mem1 mem2)
      | MemBot, MemBot -> MemBot
      | _ -> failwith "mem widen..."

    let meet _ _ =
      failwith "not implemented"


    let pp fmt mem =
      match mem with
      | MemBot -> Format.fprintf fmt "bot\n"
      | Mem mem ->
        F.fprintf fmt "[%a]\n" (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
          (fun fmt ((s, i1, i2), v) -> F.fprintf fmt " %s#%d#%d ↦ %a" s i1 i2 AbsVal.pp v)) (M.bindings mem)
  end
