module F = Format

(** An interface for the context module for context-sensitive analysis. **)

module type S = sig
  
  (* type of the context *)
  type t
  
  (* type of the abstract memory *)
  type memty
    
  (* ```empty ()``` returns empty context *)  
  val empty : unit -> t

  (* ```apply bb succs ctxt absmem``` returns a list of pairs of successors and context
    It selects successors based onthe current basicblock(bb) and current context(ctxt), 
    and it also sets the context to apply for each successor.
    The role of "apply" is to be implmented as intended by the user.
   *)
  val apply : Basicblock.t -> Basicblock.t list -> t -> memty -> (Basicblock.t * t) list
  
  val pp : Format.formatter -> t -> unit
end

