module F = Format

exception No_state

module type S =
sig
  type t
  type ctxtty
  type memty
  module CtxtM : Map.S with type key = ctxtty
  module M : Map.S

  val empty : t
  val find : Basicblock.t -> t -> memty CtxtM.t
  val mem' : Basicblock.t -> t -> bool
  val mem : Basicblock.t * ctxtty -> t -> bool
  val find_mem : Basicblock.t * ctxtty -> t -> memty
  val find_mem_opt : Basicblock.t * ctxtty -> t -> memty option
  val update : Basicblock.t * ctxtty -> memty -> t -> t
  val pp_ctxtMem : Format.formatter -> memty CtxtM.t -> unit
  val pp : Format.formatter -> t -> unit
  val iter : (Basicblock.t -> memty CtxtM.t -> unit) -> t -> unit
  val fold' : (ctxtty -> memty -> 'a -> 'a) -> memty CtxtM.t -> 'a -> 'a
end

module Make
  (Ctxt : Context.S)
  (AbsMem : AbstractMemory.S)
  : (S with type ctxtty = Ctxt.t and type memty = AbsMem.t) =
struct
  type ctxtty = Ctxt.t
  type memty = AbsMem.t

  module CtxtM = Map.Make (struct
    type t = Ctxt.t
    let compare = compare
  end)

  module M = Map.Make (struct
    type t = Basicblock.t
    let compare = compare
  end)

  type t = AbsMem.t CtxtM.t M.t

  let empty = M.empty
  let find = M.find
  let mem' = M.mem
  let iter = M.iter
  let fold' = CtxtM.fold

  let find_mem_opt (bb, ctxt) s =
    match M.find_opt bb s with
    | None -> None
    | Some ctxt_mem -> CtxtM.find_opt ctxt ctxt_mem

  let mem (bb_ctxt : Basicblock.t * Ctxt.t) s =
    let bb, ctxt = bb_ctxt in
    match M.find_opt bb s with
    | None -> false
    | Some ctxt_mem -> CtxtM.mem ctxt ctxt_mem

  let find_mem (bb_ctxt : Basicblock.t * Ctxt.t) s =
    let bb, ctxt = bb_ctxt in
    match M.find_opt bb s with
    | None -> raise No_state
    | Some ctxt_mem ->
        (match CtxtM.find_opt ctxt ctxt_mem with
         | None -> raise No_state
         | Some m -> m)

  (* IMPORTANT:
     update must REPLACE the state for (bb, ctxt),
     not join with the old one.
     join/widen is already handled in llvmAnalyzer.ml.
  *)
  let update (bb_ctxt : Basicblock.t * Ctxt.t) m s =
    let bb, ctxt = bb_ctxt in
    let ctxt_mem =
      match M.find_opt bb s with
      | None -> CtxtM.empty
      | Some ctxt_mem -> ctxt_mem
    in
    let ctxt_mem' = CtxtM.add ctxt m ctxt_mem in
    M.add bb ctxt_mem' s

  let pp_ctxtMem fmt m =
    F.fprintf fmt "[%a]"
      (F.pp_print_list
         ~pp_sep:(fun fmt () -> F.fprintf fmt "")
         (fun fmt ((ctxt : Ctxt.t), m) ->
           F.fprintf fmt "%a ↦\n%a\n" Ctxt.pp ctxt AbsMem.pp m))
      (CtxtM.bindings m)

  let pp fmt (s : t) =
    F.fprintf fmt "%a"
      (F.pp_print_list
         ~pp_sep:(fun fmt () -> F.fprintf fmt "\n\n")
         (fun fmt ((bb : Basicblock.t), m) ->
           let _ = Pp.printf ~color:Red "<%s> ↦\n" bb.bb_name in
           F.fprintf fmt "%a\n" pp_ctxtMem m))
      (M.bindings s)
end
