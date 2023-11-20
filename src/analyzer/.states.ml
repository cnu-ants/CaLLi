module F = Format

exception No_state


module type KeyType = 
  sig
    type t
    val pp : F.formatter -> t -> unit
  end

module type S = 
  sig
    type t 
    (* module Key : KeyType *)
    type ctxtty
    type memty
    module CtxtM : Map.S
    module M : Map.S

    val empty : t
    val mem : Basicblock.t * ctxtty -> t -> bool
    (* val find : KeyType.t -> t ->  *)
    val find_mem :  Basicblock.t * ctxtty -> t -> memty
    val update : Basicblock.t * ctxtty -> memty -> t ->t 
    val pp : Format.formatter -> t -> unit

  end

module Make (Ctxt : Context.S) (AbsMem : AbstractMemory.S) : (S with type ctxtty = Ctxt.t and type memty = AbsMem.t) = 
  struct

    (* module Key = Basicblock *)
    
    module CtxtM = Map.Make(struct
      type t = Ctxt.t
      let compare = compare
    end)

    module M = Map.Make(struct 
        type t = Basicblock.t
        let compare = compare
    end)

    type t = AbsMem.t CtxtM.t M.t
    type ctxtty = Ctxt.t
    type memty = AbsMem.t
    
    let empty = M.empty

    let mem (bb_ctxt : Basicblock.t * Ctxt.t) s =
      let (bb, ctxt) = bb_ctxt in
      if M.mem bb s
        then CtxtM.mem ctxt (M.find bb s)
      else false

    (* let find = M.find *)

    let find_mem (bb_ctxt : Basicblock.t * Ctxt.t) s = 
      let (bb, ctxt) = bb_ctxt in 
      try CtxtM.find ctxt (M.find bb s) with Not_found -> raise No_state

    (* let iter = M.iter *)
    
    let update (bb_ctxt : Basicblock.t * Ctxt.t) m s =
      let (bb, ctxt) = bb_ctxt in
      if M.mem bb s 
        then 
          if CtxtM.mem ctxt (M.find bb s)
            then 
              let ctxtMem = M.find bb s in
              let mem = CtxtM.find ctxt (M.find bb s) in
              let joined_mem = AbsMem.join mem m in
              let ctxtMem' = CtxtM.add ctxt joined_mem ctxtMem in
              M.add bb ctxtMem' s
          else
            let ctxtMem = CtxtM.add ctxt m (M.find bb s) in
            M.add bb ctxtMem s
      else 
        let ctxtMem = CtxtM.add ctxt m CtxtM.empty in
        M.add bb ctxtMem s
    
    (* let iter = M.iter *)

    let pp_ctxtMem fmt m =
      F.fprintf fmt "[%a]" (F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt "")
      (fun fmt ((ctxt : Ctxt.t), m) ->
        F.fprintf fmt "%a ↦ %a" Ctxt.pp ctxt AbsMem.pp m))
      (CtxtM.bindings m)

    let pp fmt (s : t) = 
      F.fprintf fmt "%a" (F.pp_print_list 
        ~pp_sep:(fun fmt () -> F.fprintf fmt "\n\n")
        (fun fmt ((bb : Basicblock.t), m) -> 
          let _ = Pp.printf ~color:Red "<%s> ↦" bb.bb_name in
          F.fprintf fmt "%a" pp_ctxtMem m))
        (M.bindings s)

  end

