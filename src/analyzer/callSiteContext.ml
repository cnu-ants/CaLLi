module F = Format

module type Elt = 
  sig
    type t
    type memty
    (* val pp : Format.formatter -> t -> unit *)
    val size : int ref
    val llmodule : Function.t Module.M.t
    val call_graph : CallGraph.t
    val pp : Format.formatter -> t -> unit
  end


module Make(Elt : Elt) = 
  struct 
    (* type elt = string *)
    type t = string list
    type memty = Elt.memty
    let size = Elt.size

    let call_graph = Elt.call_graph
    let llmodule = Elt.llmodule

    let empty () = [] 

    let pop ctxt =
      match ctxt with
      | hd :: tl -> (hd, tl)
      | [] -> failwith "pop empty context"
      (* match List.rev ctxt with
      | hd :: tl -> (hd , List.rev tl)
      | [] -> failwith "pop empty context" *)

    let push s ctxt =
      if !size = 0 
        then []
      else
        if List.length ctxt = !size
          then let (_ ,  ctxt') = pop ctxt in s::ctxt'
          else s::ctxt

    let push_back s ctxt = 
      ctxt@[s]

    let tl ctxt = 
      match List.rev ctxt with
      | hd :: _ -> hd
      | _ -> failwith "empty context"
    
    let length = List.length

    let apply (current_bb : Basicblock.t) (next_bb_list : Basicblock.t list) 
      current_ctxt _ = 
      match current_bb.term with
      | CallSite _ ->
        List.map (fun bb -> (bb, push current_bb.bb_name current_ctxt)) next_bb_list 
      | Exit _ ->        
        if !size = 0 then List.map (fun bb -> (bb, current_ctxt)) next_bb_list
        else
          if (length current_ctxt) = 0 then []
          else 
            if (length current_ctxt) = !size
              then
                let (ctxt', next_ctxt) = pop current_ctxt in
                let tl = tl current_ctxt in
                let bb' = Bbpool.find_bb tl in
                let next = List.nth (Module.next ctxt' llmodule) 0 in
                let new_ctxts = (CallGraph.front bb'.func_name call_graph llmodule) in
                if List.length new_ctxts = 0 then [(next, next_ctxt)]
                else
                List.map
                (fun bb_name -> next, (push_back bb_name next_ctxt))
                new_ctxts
            else
              let (ctxt', next_ctxt) = pop current_ctxt in
              let next = List.nth (Module.next ctxt' llmodule) 0 in
              [(next, next_ctxt)]
     | _ -> List.map (fun bb -> (bb, current_ctxt)) next_bb_list

    let pp (fmt : Format.formatter) (ctxt : t) =
      if List.length ctxt = 0 
        then Format.printf "empty" 
      else
        List.iter
        (fun s -> Format.fprintf fmt "%s::" s)
        ctxt


  end
