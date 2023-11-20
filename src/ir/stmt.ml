(**

The Stmt module is a module that adds additional information to Inst.
It includes bb_name (the name of the basicblock to which the instruction
belongs), and index (the index of the instruction within the basicblock)

 **)

type t = {bb_name:string; index:int; inst:Inst.t;}
