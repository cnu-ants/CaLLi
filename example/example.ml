module F = Format
open Calli

module MyContext = CallSiteContext.Make(
  struct 
    type t = String.t
    type memty = AbsMemory.t
    let size = ref 0

    let llmodule = Init.llmodule
    let call_graph = Init.call_graph
    let pp fmt (s : t) = F.fprintf fmt "%s" s
  end
)

module TF = 
  struct 
    
    type memty = AbsMemory.t

    let llmodule = Init.llmodule

    let abs_eval (e : Expr.t) (mem: AbsMemory.t) =
      match e with
      | ConstInt {value; _} -> AbsValue.alpha (IntLiteral value)
      | Name {name;_} -> 
        (try (match Env.find name !Env.env with 
        | Addr a -> AbsMemory.find a mem
        ) with _ -> AbsValue.top)
      | Void _ -> AbsValue.bot
      | _ -> AbsValue.top

    

    let abs_interp_stmt (stmt : Stmt.t) (mem: AbsMemory.t) : AbsMemory.t = 
      let instr = stmt.inst in
      if mem = AbsMemory.bot then mem else
      match instr with
      | ICmp {name; cond; operand0; operand1; _} ->
        let v1 = abs_eval operand0 mem in
        let v2 = abs_eval operand1 mem in
        let res =
          (match cond with
          | Eq -> AbsValue.CompOp.(v1 == v2)
          | Ne -> AbsValue.CompOp.(v1 != v2)
          | Slt -> AbsValue.CompOp.(v1 < v2)
          | Sle -> AbsValue.CompOp.(v1 <= v2)
          | Sgt -> AbsValue.CompOp.(v2 < v1)
          | Sge -> AbsValue.CompOp.(v2 <= v1)
          | Ult -> AbsValue.CompOp.(v1 < v2)
          | Uge -> AbsValue.CompOp.(v2 <= v1)
          | Ule -> AbsValue.CompOp.(v1 <= v2)
          | _ -> failwith "not implmented"
          )
        in
        let addr = (stmt.bb_name, stmt.index, 0) in
        let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
        AbsMemory.update addr res mem
      | Select {name; operand0; operand1; _;} ->
        let v1 = abs_eval operand0 mem in
        let v2 = abs_eval operand1 mem in
        let res = AbsValue.join v1 v2 in
        let addr = (stmt.bb_name, stmt.index, 0) in
        let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
        AbsMemory.update addr res mem
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
      | Alloc {name; _} -> 
          let addr = (stmt.bb_name, stmt.index, 0) in
          let a = (stmt.bb_name, stmt.index, 1) in
          let addr' = AbsValue.alpha (AddrLiteral a) in
          let mem' = AbsMemory.update addr addr' mem in
          let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
          mem'
      | Store {operand; name; _} -> 
        let v = abs_eval operand mem in
        let a = Env.find name !Env.env in
        (match a with 
        | Addr a -> 
          let a' = AbsMemory.find a mem in
          (match a' with
          | AbsAddr a'' ->
            let mem' = AbsValue.AbsAddr.fold
              (fun a mem ->  AbsMemory.update a v mem ) a'' mem
            in mem'
          | _ -> mem )
          )
      | Load {name; operand; _} -> 
          let addr = (stmt.bb_name, stmt.index, 0) in
          let res = abs_eval operand mem in
          let res' = 
            (match res with
            | AbsAddr a -> 
              AbsValue.AbsAddr.fold
              (fun a' v -> AbsValue.join v (AbsMemory.find a' mem)) a AbsValue.bot
            | AbsTop -> AbsValue.top
            | AbsBot -> AbsValue.bot
            | AbsInt _ -> failwith "load error"
            ) in
          let mem' = AbsMemory.update addr res' mem in
          let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
          mem'
      | ReturnSite {name; ty} ->
        let res = abs_eval (Expr.Name {ty=ty; name="ret"}) mem in
        let addr = (stmt.bb_name, -1, 0) in
        let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
        let mem' = AbsMemory.update addr res mem in
        mem'
      | _ -> mem



    let abs_interp_term' (term : Term.t) (mem : AbsMemory.t) = 
      if mem = AbsMemory.bot then mem else
      match term with
      | Br _ -> mem
      | CondBr _ -> mem
      | Ret {ret; bb_name} -> 
        let res = abs_eval ret mem in
        let addr = (bb_name, -1, 0) in
        let _ = Env.env := Env.add "ret" (Val.Addr addr) !Env.env in
        let mem' = AbsMemory.update addr res mem in
        mem'
      | Exit _ -> mem
      | CallSite _ -> mem
      | Switch _ -> mem
      | _ -> mem


    let transfer (bb : Basicblock.t) (mem : AbsMemory.t)  =
      let mem' = List.fold_left
        (fun mem stmt ->
          let mem'' = abs_interp_stmt stmt mem in
          mem''
        )
        mem bb.stmts 
      in
      abs_interp_term' bb.term mem'



  end

module States = States.Make (MyContext) (AbsMemory)
module Analyzer = LlvmAnalyzer.Make (AbsValue) (AbsMemory) (MyContext) (States) (TF)
module To_Dot = To_dot.Make (States)

module M = Map.Make(String)

let init_global (llm : Module.t) mem = 
  let (mem, _) = 
    List.fold_left
    (fun (mem, index) (v : Global.t) ->
      let res = TF.abs_eval v.value mem in
      let addr = ("global", index, 0) in
      let a = ("global", index, 1) in
      let addr' = AbsValue.alpha (AddrLiteral a) in      
      let mem' = AbsMemory.update addr addr' mem in
      let mem' = AbsMemory.update a res mem' in
      let _ = Env.env := Env.add v.name (Val.Addr addr) !Env.env in
      (mem', index +1)
    )
    (mem, 0) llm.globals in
  mem

let _ = 
  let _ = Analyzer.init Init.llmodule in 
  let main = Module.main Init.llmodule in
  let entry = Bbpool.find (main.function_name^"#"^"entry") !Bbpool.pool in
  let init_mem, _ = 
    List.fold_left
    (fun (mem, index) param ->
      let res = AbsValue.AbsTop in
      let addr = (entry.bb_name, -1, index) in
      let name = (Expr.typed_var_of_expr param).name in
      let _ = Env.env := Env.add name (Val.Addr addr) !Env.env in
      let mem' = AbsMemory.update addr res mem in
      (mem', index+1)
    )
    (AbsMemory.empty, 0)
    main.params
  in
  let init_mem = init_global Init.m init_mem in
  let init_states = States.update (entry, MyContext.empty ()) init_mem States.empty in
  let _ = Analyzer.analyze init_states in
  let _ = Format.printf "Analyze...\n" in
  let _ = F.printf "\n\nFin\nStates\n%a\n" States.pp !Analyzer.summary in
  let _ = F.printf "Env\n%a\n" Env.pp !Env.env in
  ()
