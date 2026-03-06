# Getting Started with CaLLi 0.4v

This guide shows the basic workflow for building a static analyzer on top of CaLLi.

In CaLLi 0.4v, users typically:
1. preprocess LLVM bitcode through Init
2. define abstract value, context, and transfer function
3. instantiate memory, states, and analyzer modules
4. seed the initial state
5. run either:
    * a normal analyzer (LlvmAnalyzer.Make), or
    * an interactive web analyzer (LlvmWebAnalyzer.Make + Monitor_web.Make)

## 1. Initialization

The `Init` module transforms input LLVM bitcode into CaLLi IR.
It also provides several optional preprocessing and transformation passes, such as:

- loop unrolling
- call instruction transformation
- select node insertion
- prune node insertion
- call graph construction

A typical initialization sequence looks like this:

```ocaml
let () =
  let _ = Init.init () in
  let _ = Init.transform_call () in
  let _ = Init.transform_select () in
  let _ = Init.transform_prune () in
  let _ = Init.make_llm () in
  let _ = Init.make_call_graph () in
  ()
```

Depending on your analysis goal, you may enable only the transformations you need.

## 2. User-Defined Components
To build an analyzer in CaLLi, users define the following core components:
1. abstract value
2. analysis context
3. transfer function
### 2.1 Abstract Value
The abstract value represents the abstraction of concrete program values.
CaLLi provides an interface for abstract domains:
```
module type S =
sig
  type elt
  type t

  val bot : t
  val top : t
  val (<=) : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t

  val alpha : elt -> string -> t

  val binop : Op.t -> t -> t -> string -> t
  val compop : Cond.t -> t -> t -> string -> t

  val pp : Format.formatter -> t -> unit
end
```
A user-defined abstract value should implement:
* lattice operations (bot, top, join, meet, widen)
* partial order (<=)
* abstraction (alpha)
* abstract semantics for binary and comparison operators

### 2.2 Analysis Context
The context module controls context-sensitive analysis.
CaLLi provides a default call-site-based context module. A typical instantiation looks like this:
```
module Ctxt =
  CallSiteContext.Make
    (struct
      type t = string
      type memty = AbsMemory.t
      let size = ref 1
      let llmodule = Init.llmodule ()
      let call_graph = Init.call_graph ()
      let pp fmt (s : t) = Format.fprintf fmt "%s" s
    end)
```
In a call-site context, the current call site is pushed into the context when a function call is encountered. This enables context-sensitive propagation across call edges.
Users may implement their own context module depending on their analysis needs.

### 2.3 Transfer Function
The transfer function defines the abstract semantics of the analysis. It takes a basic block and an abstract memory, and returns a new abstract memory.
```
module type S = sig
  type memty
  val transfer : Ast.BasicBlock.t -> memty -> memty
end
```
For example, a transfer function for binary operations may evaluate operands, apply the abstract operator, allocate/update an address, and write the result into abstract memory.

## 3. Building the Analyzer
After defining the abstract value, context, and transfer function, the next step is to instantiate abstract memory, states, and the analyzer itself.
```
module AbsMemory = AbstractMemory.Make (AbsValue)
module States = States.Make (Ctxt) (AbsMemory)
```
For standard analysis:
```
module Analyzer =
  LlvmAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF)
```
For web-monitored analysis:
```
module Analyzer =
  LlvmWebAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF)
```

## 4. Preparing the Initial State
Before running the analysis, initialize the memory and set the starting basic block.
A typical setup looks like this:
```
let llm = Init.m () in
let target_f : Function.t = Module.find target (Init.llmodule ()) in
let entry = Bbpool.find target_f.entry !Bbpool.pool in

let init_mem = Analyzer.init llm in
let init_states =
  States.update (entry, Ctxt.empty ()) init_mem States.empty
```
Here:
* Analyzer.init initializes the abstract memory
* entry is the starting basic block
* States.update seeds the initial abstract state

## 5. Running the Analysis
Set the loop bound used by the iteration algorithm:
Analyzer.LoopCounter.set_max_count 30;
Then run the analysis.
For non-web mode:
```
let _ = Analyzer.analyze_full entry init_states in
let res = !Analyzer.summary in
```
For web mode, initialize a runtime object instead:
```
let rt = Analyzer.init_runtime ~entry ~init_states
```
This runtime is then passed to the web monitor server.

## 6. Example: CLI Analyzer
A simplified analyzer entry point looks like this:
```
let web_enabled = has_flag "--web" in

if web_enabled then (
  let calli_home = get_calli_home () in

  let module Analyzer =
    LlvmWebAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF)
  in
  let module Web = Monitor_web.Make (Analyzer) in

  Analyzer.LoopCounter.set_max_count 30;

  let init_mem = Analyzer.init llm in
  let init_states =
    States.update (entry, Ctxt.empty ()) init_mem States.empty
  in
  let rt = Analyzer.init_runtime ~entry ~init_states in

  let frontend =
    if has_flag "--no-frontend" then Web.Disabled
    else if has_flag "--frontend-dev" then Web.Dev
    else Web.Static
  in

  Web.start ~frontend ~calli_home ~runtime:rt ~interface ~port ()
) else (
  let module Analyzer =
    LlvmAnalyzer.Make (AbsValue) (AbsMemory) (Ctxt) (States) (TF)
  in

  Analyzer.LoopCounter.set_max_count 30;

  let init_mem = Analyzer.init llm in
  let init_states =
    States.update (entry, Ctxt.empty ()) init_mem States.empty
  in
  let _ = Analyzer.analyze_full entry init_states in

  let s = !Analyzer.summary in
  let _ = Format.printf "ENV \n %a\n" Env.pp !Env.env in
  let _ = Format.printf "STATES \n %a\n" States.pp s in
  ()
)
```

## 7. Running the Example
CLI mode
```
dune exec ./example/analyzer.exe -- --target <function_name>
```
Web mode
Build the frontend once:
```
cd monitor
npm install
npm run build
```
Then run:
```
dune exec ./example/analyzer.exe -- --web --calli-home /path/to/CaLLi --target <function_name>
```
Or with an environment variable:
```
export CALLI_HOME=/path/to/CaLLi
dune exec ./example/analyzer.exe -- --web --target <function_name>
```
Open the monitor in your browser:
http://localhost:8080
Web mode without frontend
```
dune exec ./example/analyzer.exe -- --web --no-frontend --calli-home /path/to/CaLLi --target <function_name>
```
Web mode with frontend development server
```
dune exec ./example/analyzer.exe -- --web --frontend-dev --calli-home /path/to/CaLLi --target <function_name>
```

## 8. Notes
* --calli-home specifies the root directory of the CaLLi repository.
* If --calli-home is omitted, CaLLi uses the CALLI_HOME environment variable.
* The default web server port is 8080.
* The web monitor is optional; standard CLI analysis works independently.

