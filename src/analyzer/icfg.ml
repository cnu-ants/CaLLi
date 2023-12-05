module type S =
  sig
    type t 
    val pp : Format.formatter -> t -> unit
  end

module Make (AbsMem : AbstractMemory.S ) ( Ctxt : Context.S with type memty = AbsMem.t) =
  struct

    module M = Map.Make(String)
    type t = (String.t list) M.t

    let find = M.find
    let empty = M.empty
    let add = M.add
    let iter = M.iter
    
    let make (m : Function.t Module.M.t) : t =
      let icfg = Module.fold 
      (fun _ (func : Function.t) icfg ->
        M.union (fun _ s1 _ -> Some s1) icfg func.cfg
      )
      m
      empty
      in
      let icfg = M.fold
        (fun bb_name _ icfg -> 
          let bb = Bbpool.find bb_name !Bbpool.pool in
          match bb.term with
          | CallSite {callee; _} ->
            let f = Module.find bb.func_name m in
            let icfg = 
              if Bbpool.mem (callee^"#entry") !Bbpool.pool then
                add bb_name ((callee^"#entry")::(M.find bb_name f.cfg)) icfg
              else icfg in
            let next = List.nth (Cfg.next bb f.cfg) 0 in
            if Bbpool.mem (callee^"#exit") !Bbpool.pool then
              add (callee^"#exit") (next.bb_name::(M.find (callee^"#exit") icfg)) icfg
            else icfg 
          | _ -> icfg
        )
        icfg icfg 
      in
      icfg

    let prev (bb : Basicblock.t) m : Basicblock.t list = 
      let f = Module.find bb.func_name m in
      Cfg.fold
      (fun bb_name nexts lst -> 
        if List.mem bb.bb_name nexts 
        then (Bbpool.find_bb bb_name)::lst else lst
      )
      f.cfg []

    let next_fallback (bb : Basicblock.t) m : Basicblock.t list = 
      let f = Module.find bb.func_name m in
      let cfg = f.cfg in
      Cfg.next bb cfg

    
    let next_intra (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) _ m : (Basicblock.t * Ctxt.t) list =
      let f = Module.find bb.func_name m in
      let cfg = f.cfg in
      let next_bb_list = Cfg.next bb cfg in 
      Ctxt.apply bb next_bb_list ctxt mem


    let next (bb : Basicblock.t) (ctxt : Ctxt.t) (mem : AbsMem.t) icfg m : (Basicblock.t * Ctxt.t) list =
      (* let bb_list = M.find bb.bb_name icfg |> 
        List.map (fun b -> Bbpool.find b !Bbpool.pool)
        in
      Ctxt.apply bb bb_list ctxt mem *)
      let bb_list = M.find bb.bb_name icfg |> List.map (fun b -> Bbpool.find b !Bbpool.pool) in
      let next_bb_list = match bb.term with
      | CallSite _ ->
        let fallbacks = next_fallback bb m in
        List.filter_map
        (fun v -> if List.mem v fallbacks then None else Some v)
        bb_list
      | _ -> bb_list
      in
      if next_bb_list != [] then
        Ctxt.apply bb next_bb_list ctxt mem
      else 
        let fallbacks = next_fallback bb m in
        List.map
        (fun v -> (v, ctxt))
        fallbacks

    let pp _ icfg = 
      M.iter
      (fun k v ->
        let _ = Format.printf "%s -> " k in
        let _ = List.iter (fun s -> Format.printf "%s ," s) v in
        Format.printf "\n"
      )
      icfg
  end
