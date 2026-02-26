module F = Format

module type S =
  sig
    type t
    type valty

    val empty : t
    val bot : t
    val is_bot : t -> bool
    val find_opt : string -> t -> valty option
    val find : string -> t -> valty
    val update : string -> valty -> t -> t
    val (<=) : t -> t -> bool
    val join : t -> t -> t
    val meet : t -> t -> t
    val widen : t -> t -> t

    (* NEW: structured view *)
    val bindings : t -> (string * valty) list

    val pp : Format.formatter -> t -> unit
  end

module Make(AbsVal : AbstractDomain.S) : (S with type valty = AbsVal.t) =
struct
  type key = string
  module M = Map.Make(struct type t = key let compare = compare end)

  type t = MemBot | Mem of AbsVal.t M.t
  type valty = AbsVal.t

  let empty = Mem (M.empty)
  let bot = MemBot

  let is_bot m =
    if m = empty then true
    else if m = bot then true
    else false

  let find_opt x mem =
    match mem with
    | Mem mem -> M.find_opt x mem
    | MemBot -> None

  let find x mem =
    match mem with
    | Mem mem ->
        if x = "" then AbsVal.top
        else (try M.find x mem with _ -> AbsVal.top)
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
    | MemBot, _ -> mem2
    | _, MemBot -> mem1
    | Mem mem1, Mem mem2 ->
        let mem = Mem (M.union (fun _ v1 v2 -> Some AbsVal.(join v1 v2)) mem1 mem2) in
        if mem = empty then MemBot else mem

  let widen mem1 mem2 =
    match mem1, mem2 with
    | Mem mem1, Mem mem2 ->
        Mem (M.union (fun _ v1 v2 -> Some (AbsVal.widen v1 v2)) mem1 mem2)
    | MemBot, MemBot -> MemBot
    | MemBot, Mem _ -> mem2
    | _ -> failwith "mem widen..."

  let meet _ _ = failwith "not implemented"

  (* NEW *)
  let bindings mem =
    match mem with
    | MemBot -> []
    | Mem m -> M.bindings m

  let pp fmt mem =
    match mem with
    | MemBot -> Format.fprintf fmt "bot\n"
    | Mem mem ->
        F.fprintf fmt "[%a]\n"
          (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
             (fun fmt (s, v) -> F.fprintf fmt " %s ↦ %a" s AbsVal.pp v))
          (M.bindings mem)
end
