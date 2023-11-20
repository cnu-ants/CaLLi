module type S =
  sig
    type elt
    type t

    val bot : t
    val top : t
    val (<=) : t -> t -> bool
    val join : t -> t -> t
    val meet : t -> t -> t
    val alpha : elt -> t
    val pp : Format.formatter -> t -> unit
    val widen : t -> t -> t
    
  end