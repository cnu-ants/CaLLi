module F = Format

type t = Addr of (string * int * int) 
(* | AbsVal of AbsValue.t *)

let pp _ v =
  match v with
  | Addr (s, i1, i2) -> F.printf "%s#%d#%d" s i1 i2 
