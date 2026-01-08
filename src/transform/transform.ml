open Util


(** 
    The Transform module utilize the LLVM-OCaml binding
    to convert LLVM IR into CaLLI IR.
 **)



let new_name = ref 0


(* convert Llvm.Icmp.t to Cond.t *)
let transform_cond cond : Cond.t =
  match Llvm.icmp_predicate cond with
  | Some (Llvm.Icmp.Eq) -> Cond.Eq
  | Some (Llvm.Icmp.Ne) -> Cond.Ne
  | Some (Llvm.Icmp.Ugt) -> Cond.Ugt
  | Some (Llvm.Icmp.Ult) -> Cond.Ult
  | Some (Llvm.Icmp.Sgt) -> Cond.Sgt
  | Some (Llvm.Icmp.Sge) -> Cond.Sge
  | Some (Llvm.Icmp.Slt) -> Cond.Slt
  | Some (Llvm.Icmp.Sle) -> Cond.Sle
  | Some (Llvm.Icmp.Uge) -> Cond.Uge
  | Some (Llvm.Icmp.Ule) -> Cond.Ule
  | None -> failwith "transform cond error"


let rec transform_lltype lltype =
  let res = match Llvm.classify_type lltype with
  | Integer -> 
      let bitwidth = (Llvm.integer_bitwidth lltype) in 
      Type.Integer {bitwidth=bitwidth}
  | Pointer ->  
      let element_type = transform_lltype (Llvm.element_type lltype) in 
      Type.Pointer {ty=element_type}
  | Vector ->  
      let element_type = transform_lltype (Llvm.element_type lltype) in
      let vector_size = Llvm.vector_size lltype in
      Type.Vector {size=vector_size; ty=element_type}
  | Label -> Type.Label
  | Float -> Type.Float
  | Array -> 
      let element_ty = transform_lltype (Llvm.element_type lltype) in
      let size = Llvm.array_length lltype in
      Type.Array {size=size; ty=element_ty}
  | Void | Half ->  Type.Void
  | _ -> Undef
  in
  res

let transform_expr_type expr : Type.t = 
  let lltype = Llvm.type_of expr in
  let res = transform_lltype lltype in 
  res


let rec transform_e e func_name : Expr.t = 
  let lltype = Llvm.type_of e in
  let ty = transform_lltype lltype in
  let res = if ty = Type.Void then Expr.Void {ty=Type.Void} 
  else
    let str = (Llvm.string_of_llvalue e) in
    match Llvm.classify_value e with
    | Llvm.ValueKind.ConstantInt -> 
      Expr.ConstInt {ty=ty; value=get_int str}
    (*| Llvm.ValueKind.ConstantFP -> Expr.ConstFP {ty=get_type str; value=get_float str}
    | Llvm.ValueKind.ConstantVector
    | Llvm.ValueKind.ConstantDataVector -> 
      let element_ty = transform_lltype (Llvm.element_type lltype) in
      let vector = 
        List.fold_left
        (fun vector expr -> 
          (match element_ty with
          | Integer _ -> vector@[Expr.ConstInt {ty=element_ty; value=get_int expr}]
          | Float -> vector@[Expr.ConstFP {ty=element_ty; value=get_float expr}]
          | _ -> failwith "UB"
          )
        )  
        []
        (get_vec_list e)
      in
      Expr.Vector {ty=ty; value=vector}
    (* TODO : 'zeroinitializer' 
      can be used to zero initialize a value to zero of any type,
      including scalar and aggregate types 
      but not only for a vector type *)
    | Llvm.ValueKind.ConstantAggregateZero -> Expr.Undef
      (* let element_ty = transform_lltype (Llvm.element_type lltype) in
      let size = Llvm.vector_size lltype in
      let vector = 
        List.init 
        size
        (fun _ -> 
          (match element_ty with
          | Integer _ -> Expr.ConstInt {ty=element_ty; value= Z.of_int 0}
          | Float -> Expr.ConstFP {ty=element_ty; value=0.}
          | _ -> Expr.ConstInt {ty=element_ty; value= Z.of_int 0}
          )
        )
      in
      Expr.Vector {ty=ty; value=vector} *)
    | Llvm.ValueKind.ConstantArray
    | Llvm.ValueKind.ConstantDataArray ->
      let element_ty = transform_lltype (Llvm.element_type lltype) in
      let size = Llvm.array_length lltype in
      let array = 
        List.init
        size
        (fun index -> 
          (match element_ty with
          | Integer _ -> 
          (match (Llvm.string_of_const e) with 
          | Some str -> Expr.ConstInt {ty=element_ty; value=Z.of_int (Char.code (String.get str index))}
          | None -> Expr.ConstInt {ty=element_ty; value=get_int (List.nth (get_list_list e) index)})
          | Float -> Expr.ConstFP 
            {ty=element_ty; value=float_of_string (Llvm.string_of_llvalue (Llvm.operand e index))}
          | Vector _ -> transform_e (Llvm.operand e index) func_name
          | Array _ -> transform_e (Llvm.operand e index) func_name
          | _ -> Undef (*failwith "constant array"*)
        )
      )
      in
      Expr.Array {ty=ty; value=array}*)
    (* | Llvm.ValueKind.ConstantStruct ->  *)
    | Llvm.ValueKind.GlobalVariable -> Expr.Name {ty=ty; name=get_name e}
    | Llvm.ValueKind.BasicBlock -> Expr.Name {ty=ty; name=func_name^"#"^get_bbname e}
    | Llvm.ValueKind.ConstantExpr -> Expr.Undef
    (* failwith (Format.asprintf "constexpr %s"(Llvm.string_of_llvalue e)) *)
    | _ -> Expr.Name {ty=ty; name=func_name^(get_name e)}
    in
    
    res


let transform_binop op = 
  (* let _ = Format.printf "transform binop\n@." in *)

  match op with
  | Llvm.Opcode.Add  -> Op.Add
  | Llvm.Opcode.FAdd -> Op.FAdd
  | Llvm.Opcode.Sub  -> Op.Sub
  | Llvm.Opcode.FSub -> Op.FSub
  | Llvm.Opcode.Mul  -> Op.Mul
  | Llvm.Opcode.FMul -> Op.FMul
  | Llvm.Opcode.SDiv -> Op.SDiv
  | Llvm.Opcode.UDiv -> Op.UDiv
  | Llvm.Opcode.FDiv -> Op.FDiv
  | Llvm.Opcode.SRem -> Op.SRem
  | Llvm.Opcode.URem -> Op.URem
  | Llvm.Opcode.FRem -> Op.FRem
  | Llvm.Opcode.Shl  -> Op.Shl
  | Llvm.Opcode.LShr -> Op.LShr
  | Llvm.Opcode.AShr -> Op.AShr
  | Llvm.Opcode.And  -> Op.And
  | Llvm.Opcode.Or   -> Op.Or
  | Llvm.Opcode.Xor  -> Op.Xor
  | _ -> failwith "Not a OP"

let rec transform_args instr func_name num_args : Expr.t list = 
  if num_args = 0 then [(transform_e (Llvm.operand instr 0) func_name)]
  else (transform_args instr func_name (num_args -1))@
  [(transform_e (Llvm.operand instr num_args) func_name)]


let transform_instr instr func_name: Inst.t =
  (*let _ = Format.printf "%s@." (Llvm.string_of_llvalue instr) in*)
  let op = Llvm.instr_opcode instr in 
  let res = match op with
  (* | Llvm.Opcode.FNeg  *)
  | Llvm.Opcode.Add
  | Llvm.Opcode.FAdd
  | Llvm.Opcode.Sub
  | Llvm.Opcode.FSub
  | Llvm.Opcode.Mul
  | Llvm.Opcode.FMul
  | Llvm.Opcode.SDiv
  | Llvm.Opcode.UDiv
  | Llvm.Opcode.FDiv
  | Llvm.Opcode.SRem
  | Llvm.Opcode.URem
  | Llvm.Opcode.FRem
  | Llvm.Opcode.Shl
  | Llvm.Opcode.LShr
  | Llvm.Opcode.AShr
  | Llvm.Opcode.And
  | Llvm.Opcode.Or
  | Llvm.Opcode.Xor     -> Inst.BinaryOp 
                        {name=func_name^(get_name instr); 
                        op=(transform_binop op); 
                        operand0=(transform_e (Llvm.operand instr 0) func_name); 
                        operand1=(transform_e (Llvm.operand instr 1) func_name);
                        ty=transform_expr_type (Llvm.operand instr 1)}
  | Llvm.Opcode.Alloca  ->
                        Inst.Alloc 
                        {name=func_name^(get_name instr);
                        ty=transform_expr_type instr}
  | Llvm.Opcode.Store   ->
                        let n = 
                          if Util.is_global (get_name (Llvm.operand instr 1)) 
                          then (get_name (Llvm.operand instr 1))
                          else func_name^(get_name (Llvm.operand instr 1)) 
                        in
                        Inst.Store 
                        {operand=(transform_e (Llvm.operand instr 0) func_name); 
                        name=n;
                        ty = transform_expr_type (Llvm.operand instr 1)}
  | Llvm.Opcode.Load    -> Inst.Load 
                        {name=(func_name^(get_name instr)); 
                        operand=(transform_e (Llvm.operand instr 0) func_name);
                        ty = transform_expr_type instr}
  | Llvm.Opcode.ICmp    -> Inst.ICmp 
                        {name=(func_name^(get_name instr)); 
                        cond=(transform_cond instr);
                        operand0=(transform_e (Llvm.operand instr 0) func_name); 
                        operand1=(transform_e (Llvm.operand instr 1) func_name);
                        ty=transform_expr_type instr}
  | Llvm.Opcode.Select  -> Inst.Select 
                        {name=(func_name^(get_name instr));
                        cond=(transform_e (Llvm.operand instr 0) func_name); 
                        operand0=(transform_e (Llvm.operand instr 1) func_name); 
                        operand1=(transform_e (Llvm.operand instr 2) func_name);
                        ty=transform_expr_type (Llvm.operand instr 1)}
  | Llvm.Opcode.Call -> 
  let args = if (Llvm.num_operands instr) = 1 then [] 
                          else (transform_args instr func_name ((Llvm.num_operands instr) -2)) in
                        Inst.Call
                        {name=(func_name^(get_name instr));
                        callee=(get_fname (Llvm.operand instr ((Llvm.num_operands instr) -1)));
                        args=args;
                        ty=transform_expr_type instr}
  (*| Llvm.Opcode.GetElementPtr -> 
                        let num_index = Llvm.num_operands instr in
                        let index_list = 
                          if num_index = 1 then []
                          else List.init (num_index - 1) (fun i -> transform_e (Llvm.operand instr (i+1)) func_name)
                        in
                        Inst.GetElementPtr
                        {name=(func_name^(get_name instr));
                        ty=transform_expr_type instr;
                        operand=(transform_e (Llvm.operand instr 0) func_name);
                        index=index_list}
  | Llvm.Opcode.BitCast -> Inst.BitCast
                        {name=(func_name^(get_name instr));
                        operand=(transform_e (Llvm.operand instr 0) func_name);
                        ty=transform_expr_type instr;}
  *)| Llvm.Opcode.ZExt -> Inst.Zext
                        {name=(func_name^(get_name instr));
                        operand=(transform_e (Llvm.operand instr 0) func_name);
                        ty=transform_expr_type instr;}
  | Llvm.Opcode.SExt -> Inst.Sext
                        {name=(func_name^(get_name instr));
                        operand=(transform_e (Llvm.operand instr 0) func_name);
                        ty=transform_expr_type instr;}
  | Llvm.Opcode.Trunc -> Inst.Trunc
                        {name=(func_name^(get_name instr));
                        operand=(transform_e (Llvm.operand instr 0) func_name);
                        ty=transform_expr_type instr;}
  | Llvm.Opcode.PtrToInt -> Inst.PtrToInt
                            {name=(func_name^(get_name instr));
                            operand=(transform_e (Llvm.operand instr 0) func_name);
                            ty=transform_expr_type instr;}
  | Llvm.Opcode.IntToPtr -> Inst.IntToPtr
                            {name=(func_name^(get_name instr));
                            operand=(transform_e (Llvm.operand instr 0) func_name);
                            ty=transform_expr_type instr;}
  | _ -> Inst.Other
  in
  res

let transform_term term func_name bb_name: Term.t =
  match Llvm.instr_opcode term with
  | Llvm.Opcode.Br -> 
    (match Llvm.is_conditional term with
    | true -> Term.CondBr 
      {bb_name=bb_name;
      cond=(transform_e (Llvm.operand term 0) func_name); 
      succ0=(transform_e (Llvm.operand term 2) func_name); 
      succ1=(transform_e (Llvm.operand term 1) func_name)}
    | false -> Term.Br 
      {bb_name=bb_name; succ=transform_e (Llvm.operand term 0) func_name})
  | Llvm.Opcode.Ret -> 
      if (Llvm.num_operands term) = 0 then 
        Term.Ret {bb_name=bb_name; ret=(Expr.Void {ty=Type.Void})}
      else 
        Term.Ret {bb_name=bb_name; ret=(transform_e (Llvm.operand term 0) func_name)}

  | Llvm.Opcode.Switch -> Term.Switch 
    {bb_name=bb_name;
    cond=(transform_e (Llvm.operand term 0) func_name);
    default_succ=(transform_e (Llvm.operand term 1) func_name);
    succ=List.init (((Llvm.num_operands term) - 2) / 2) 
      (fun n -> (transform_e (Llvm.operand term ((n+1)*2)) func_name, (transform_e (Llvm.operand term ((n+1)*2+1)) func_name;)))
    }
  | _ -> Term.Other

(*supported llvm 14*)
(*
let get_location instr : Stmt.Loc.t option = 
  let loc = 
    Array.fold_left
    (fun loc (_, md) -> 
      let mdkind = Llvm_debuginfo.get_metadata_kind md in
      match mdkind with
      | DILocationMetadataKind -> 
        let line = (Llvm_debuginfo.di_location_get_line ~location:md) in
        let col = (Llvm_debuginfo.di_location_get_column ~location:md) in
        Some (line, col)
      | _ -> loc
    )
    None
    (Llvm.global_copy_all_metadata instr)
  in
  match loc with
  | None -> None
  | Some (line, col) -> Some {line=line; col=col;}
*)

let get_location _ : Stmt.Loc.t option = 
  None

let transform_bb llbb func_name : Basicblock.t =
  let bb_name = func_name^"#"^(get_bbname (Llvm.value_of_block llbb)) in
  let (stmt_list, _ ) =
    Llvm.fold_left_instrs
    (fun (stmt_list, index) lli -> 
      match Llvm.instr_opcode lli with
      | Llvm.Opcode.Br
      | Llvm.Opcode.Ret 
      | Llvm.Opcode.Switch -> (stmt_list, index)
      | _ ->  
        let inst = (transform_instr lli func_name) in
        let loc = get_location lli in
        let stmt : Stmt.t = 
          {bb_name=bb_name; index=index; inst=inst; loc=loc} 
        in
        (stmt_list@[stmt], index+1)
    )
    ([], 0)
    llbb
  in
  let term =
    match Llvm.block_terminator llbb with
    | Some term_inst -> transform_term term_inst func_name bb_name
    | _ -> failwith "Unreachable"
  in
  {func_name=func_name; bb_name=bb_name; stmts=stmt_list; 
  term=term; loc=(get_bbname (Llvm.value_of_block llbb))}

    
let transform_bbpool llf =
  let func_name = get_fname llf in
  let _ = 
    Llvm.iter_blocks
    (fun llbb -> 
      let _ = Bbpool.pool := Bbpool.add (func_name^"#"^(get_llbb_name llbb))
              (transform_bb llbb func_name) !Bbpool.pool in
      ()
    )
    llf
  in
  let exit_bb : Basicblock.t = 
    {func_name=func_name; bb_name=func_name^"#exit"; 
    stmts=[]; term=Exit {bb_name=func_name^"#exit";}; loc=""} in 
  let _ = Bbpool.pool := Bbpool.add (func_name^"#exit") exit_bb !Bbpool.pool in
  ()


let transform_cfg llf : Cfg.t =
  let func_name = get_fname llf in
  let cfg = 
    Llvm.fold_left_blocks
    (fun (cfg_map) llbb ->
      let bb_name = (func_name^"#"^(get_llbb_name llbb)) in
      (match Llvm.block_terminator llbb with
      | Some term -> 
        (match Llvm.instr_opcode term with
        | Llvm.Opcode.Br -> 
          let succ =
          (match Llvm.is_conditional term with
          | false -> [(func_name^"#"^(get_llbb_name (Llvm.successor term 0)))]
          | true -> [(func_name^"#"^(get_llbb_name (Llvm.successor term 0))); 
                      (func_name^"#"^(get_llbb_name (Llvm.successor term 1)))])
          in
          Cfg.add bb_name succ cfg_map
        | Llvm.Opcode.Ret -> Cfg.add bb_name [(func_name^"#exit")] cfg_map
        | Llvm.Opcode.Switch -> 
          let succ = List.init ((Llvm.num_operands term) / 2)
           (fun n -> (func_name^"#"^(get_llbb_name (Llvm.successor term n))))
          in
          Cfg.add bb_name succ cfg_map
        (* | Llvm.Opcode.Unreachable ->  *)
        | _ -> Cfg.add bb_name [] cfg_map)
      | _ -> failwith "Unreachable")
    )
    (Cfg.empty)
    llf
  in
  let cfg' = Cfg.add (func_name^"#exit") [] cfg in
  cfg'


let transform_func llf (*: Function.t*) =
  let func_name = get_fname llf in
  let _ = transform_bbpool llf in
  let cfg = transform_cfg llf in
  let entry_name = 
    if Array.length (Llvm.basic_blocks llf) = 0 then ""
    else
      let entry = Llvm.entry_block llf in
      func_name^"#"^(get_bbname (Llvm.value_of_block entry)) 
  in
  let llparams = Llvm.params llf in
  let params = Array.fold_left 
    (fun params' param -> 
      let p = transform_e param func_name in params'@[p]) 
    [] llparams 
  in
  let func : Function.t = {function_name=func_name; cfg=cfg; params=params; metadata=Empty; entry=entry_name} in
  let meta = Transform_meta.make_alias func in
  {func with metadata=meta}


let transform_module llm : Module.t =
  let glist = 
    Llvm.fold_left_globals
    (fun glist v -> 
          let var : Global.t = 

        {name=get_name v; ty=transform_expr_type v; value=transform_e (Llvm.operand v 0) ""} in
      glist@[var])
    []
    llm
  in
  let function_map = 
    Llvm.fold_left_functions
    (fun (func_map) func -> 
      let f = transform_func func in 
      Module.M.add (f.function_name) f func_map
    )
    Module.M.empty
    llm
  in
  let llmodule : Module.t = {function_map = function_map; globals=glist} in
  llmodule
