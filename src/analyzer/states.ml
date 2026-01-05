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
    (* val find : KeyType.t -> t ->  *)
    val find_mem :  Basicblock.t * ctxtty -> t -> memty
    val find_mem_option :  Basicblock.t * ctxtty -> t -> memty option
    val update : Basicblock.t * ctxtty -> memty -> t ->t 
    val pp_ctxtMem : Format.formatter -> memty CtxtM.t -> unit
    val pp : Format.formatter -> t -> unit
    val iter : (Basicblock.t -> memty CtxtM.t -> unit) -> t -> unit
    val fold' : (ctxtty -> memty -> 'a -> 'a) -> memty CtxtM.t -> 'a -> 'a

  end
  

module Make (Ctxt : Context.S) (AbsMem : AbstractMemory.S) : (S with type ctxtty = Ctxt.t and type memty = AbsMem.t) = 
  struct
    
    type ctxtty = Ctxt.t
    type memty = AbsMem.t

    module CtxtM = Map.Make(struct
      type t = Ctxt.t
      let compare = compare
    end)

    module M = Map.Make(struct 
        type t = Basicblock.t
        let compare = compare
    end)

    type t = AbsMem.t CtxtM.t M.t

    
    let find = M.find 
    let mem' = M.mem
    let empty = M.empty
    let iter = M.iter

    let fold' = CtxtM.fold

    let mem (bb_ctxt : Basicblock.t * Ctxt.t) s =
      let (bb, ctxt) = bb_ctxt in
      if M.mem bb s
        then CtxtM.mem ctxt (M.find bb s)
      else false

    (* let find = M.find *)

    let find_mem (bb_ctxt : Basicblock.t * Ctxt.t) s = 
      let (bb, ctxt) = bb_ctxt in 
      try CtxtM.find ctxt (M.find bb s) with Not_found -> raise No_state

    let find_mem_option (bb, ctxt) s = 
      try CtxtM.find_opt ctxt (M.find bb s) with Not_found -> None

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
        (* F.fprintf fmt "%a ↦\n" Ctxt.pp ctxt)) *)
          F.fprintf fmt "%a ↦\n%a\n" Ctxt.pp ctxt AbsMem.pp m
        ))
      (CtxtM.bindings m)

(*
    let pp_ctxtMem fmt m =
      F.fprintf fmt "[%a]" 
      (F.pp_print_list
      ~pp_sep:(fun fmt () -> F.fprintf fmt "")
      (fun fmt ((ctxt : Ctxt.t), _) ->
        F.fprintf fmt "%a\n" Ctxt.pp ctxt))
      (CtxtM.bindings m)
*)
    let pp fmt (s : t) = 
      F.fprintf fmt "%a" (F.pp_print_list 
        ~pp_sep:(fun fmt () -> F.fprintf fmt "\n\n")
        (fun fmt ((bb : Basicblock.t), m) -> 
          let _ = Pp.printf ~color:Red "<%s> ↦\n" bb.bb_name in
          F.fprintf fmt "%a\n" pp_ctxtMem m))
        (M.bindings s)

  end

