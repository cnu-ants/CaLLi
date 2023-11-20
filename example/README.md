# Getting Started

## User Definition

1) Abstract Value

The abstract value in CaLLi represents the abstraction of values within a program. CaLLi provides infterface for abstract value module.

```
module type S = 
sig
  type elt type t
  val bot : t
  val top : t
  val (<=) : t -> t -> bool
  val join : t -> t -> t 
  val meet : t -> t -> t
  val alpha : elt -> t 
  val widen : t -> t -> t
end
```

The abstract value includes the smallest value 'bot' and biggest value 'top'. 
Additionally, functions for calculating partial order(⊑) between values, as well as join, meet and widen functions, need to be implemented.


2) Analysis Context

The analysis context module support context-sensitive analysis. 

```
module type S = sig
  type t type memty
  val empty : t
  val apply : Ast.BasicBlock.t -> Ast.BasicBlock.t list -> t -> memty -> (Ast.
end
```

The 'apply' functions to create new contexts using the current basicblock, context, 
and abstract memory which is the analysis result. It then applies the new context 
to the next basicblocks or selects the next basicblocks based on the new context.

CaLLi provides a callsite context module by default. 
The following is part of the apply function within the callsite context in Calli.

```
  ...
match current_bb.term with
| CallSite _ ->
  List.map (fun bb -> (bb, push current_bb.bb_name current_ctxt)) next_bb_list 
  ...     
```

In the callsite context, when encountering a function call in the form of a 
CallSite command, the apply function generates a new context by pushing the 
current basic block name onto the current context (call stack). It then applies
 this new context to the successors in the next_bb_list of the current basic 
 block.

Users can implement the apply function according to their analysis goals to create a context module tailored to their specific needs.


3) Transfer Function

The transfer function in CaLLi plays a crucial role in the analysis process. It is an implementation of abstract semantics. It takes a basicblock and abstract memory as input and returns a new abstract memory.

```
module type S = sig
  type memty
  val transfer : Ast.BasicBlock.t -> memty -> memty
end
```

For example, the following code represents the transfer function for BinaryOperation commands. It calculates the values of each operand, performs the operation, and then updates the Env and AbsMemory with the resulting value.

```
| BinaryOp {name; op; operand0; operand1; _} ->
  let v1 = abs_eval operand0 mem in
  let v2 = abs_eval operand1 mem in
  let res : AbsValue.t = 
    (match op with
    | Add -> AbsValue.BinOp.(v1 + v2)
    | Sub -> AbsValue.BinOp.(v1 - v2)
    | Mul -> AbsValue.BinOp.(v1 * v2)
    | SDiv -> AbsValue.BinOp.(v1 / v2)
    | _ -> AbsValue.top
    )
  in
  let addr = (stmt.bb_name, stmt.index, 0) in
  let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
  AbsMemory.update addr res mem
```


## Building Analyzer

Utilizing the provided Functor in Calli, it is essential to generate the absmemory 
and states modules crucial for the creation of the analyzer.

```
module MyAbsMemory = AbstractMemory.Make(AbsValue)
module MyStates = States.Make (MyContext) (MyAbsMemory)
```

Finally, users can create an analyzer by using the Functor of the CalliAnalyzer module.

```
module Analyzer = LlvmAnalyzer.Make (MyAbsValue) (MyAbsMemory) (MyContext) (MyStates) (MyTF)
```


## Performing Analysis

The Transform module facilitates the conversion of input LLVM bitcode into Calli 
Intermediate Representation (IR) recognized by the Calli analyzer. Additionally, 
users have the flexibility to employ various transformation features provided by 
Calli, such as 'call instruction transformation,' 'prune node insertion,' 'select 
node insertion,' 'exit node insertion,' and more, according to their specific 
requirements.

```
let m : Module.t = 
  let llctx = Llvm.create_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  let calli_module1 = Transform.transform_module llm in  
  let calli_module2 = Transform2.transform_call calli_module1 in  
  let calli_module3 = Transform_select.transform_select calli_module12 in    
  let calli_module4 = Transform3.add_prune_node calli_module13 in    
  calli_module4
```

The user can perform analysis using the analyzer created through the Functor. 
Initially, the analyzer is initialized with the AST transformed into Calli IR.

```
  let _ = Analyzer.init m
```

Set the starting basicblock for analysis and configure the initial the abstract memory.


```
let entry = Bbpool.find (function_name^"#"^"entry") !Bbpool.pool in
let init_mem = AbsMemory.empty in
let init_states = MyStates.update (entry, MyContext.empty ()) init_mem MyStates.empty in
```

Ultimately, analysis can be performed through the analyze module of the analyzer.

```
let _ = Analyzer.analyze entry init_states
```
