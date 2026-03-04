module type S =
  
  sig
    type elt
    type t

    val bot : t
    val top : t
    val (<=) : t -> t -> bool
    val join : t -> t -> t
    val meet : t -> t -> t
    (* val widen : t -> t -> t *)
    val widen : string -> t -> t -> t

    val alpha : elt -> string -> t

    val binop : Op.t -> t -> t -> string -> t
    val compop : Cond.t -> t -> t -> string -> t

    val pp : Format.formatter -> t -> unit

    val is_top : t -> bool
    val equal : t -> t -> bool
    val is_singleton : t -> bool
    val extract_value_string : t -> string option
  end
