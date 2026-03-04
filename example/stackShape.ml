open Calli

module AbsValue = AbsValue

(* 배열 정보 *)
type array_info = {
  base_offset : int;   (* %arg_esp 기준 오프셋, ex: -48 *)
  size : int;          (* 배열 원소 개수 *)
  element_size : int;  (* 원소 크기, 4 고정 *)
}

(* 스택 엔트리 *)
type stack_entry =
  | Variable
  | Array of array_info

(* 스택 슬롯: (start_offset, end_offset, entry) *)
type stack_slot = {
  start_offset : int;
  end_offset : int;
  entry : stack_entry;
}

(* variable set: 주소값(ex: 99952) -> 변수이름 리스트 *)
(* group_by_value_with_env 결과를 그대로 활용 *)
type var_set = (string * string list) list

(* Step 3: add instruction 순회하며 배열 탐지 *)
let detect_arrays 
  (target_f : Function.t)
  (var_set : var_set)
  (s2_exit_mem : AbsMemory.t)
  : array_info list =

  (* variable set의 변수 이름들을 flat하게 모아서 빠른 조회용 Set 구성 *)
  let var_names : (string, string) Hashtbl.t = Hashtbl.create 100 in
  List.iter (fun (addr_str, names) ->
    List.iter (fun name -> Hashtbl.replace var_names name addr_str) names
  ) var_set;

  let results = ref [] in

  Cfg.iter_from_entry (fun bb_name ->
    let bb = Bbpool.find bb_name !Bbpool.pool in
    List.iter (fun (stmt : Stmt.t) ->
      match stmt.inst with
      | BinaryOp {op = Add; operand0; operand1; _} ->
        (* op1이 variable set에 속하는지 확인 *)
        let op1_in_varset = match operand0 with
          | Expr.Name {name; _} -> Hashtbl.mem var_names name
          | _ -> false
        in
        if op1_in_varset then begin
          (* op2의 interval 분석값 조회 *)
          let op2_absval = match operand1 with
            | Expr.Name {name; _} ->
              (try
                let addr = Env.find name !Env.env in
                Some (AbsMemory.find addr s2_exit_mem)
              with _ -> None)
            | _ -> None
          in
          match op2_absval with
          | Some (AbsValue.AbsInt (AbsInterval.IntInterval {
              min = AbsInterval.I lo; 
              max = AbsInterval.I hi})) 
            when not (Z.equal lo hi) ->  (* singleton 제외 *)
            let range = Z.to_int (Z.sub hi lo) in
            let size = range / 4 + 1 in
            let base_addr_str = match operand0 with
              | Expr.Name {name; _} -> Hashtbl.find var_names name
              | _ -> ""
            in
            (* base_addr_str은 "99952" 같은 값, offset = addr - 100000 *)
            let base_offset = 
              (try int_of_string base_addr_str - 100000
               with _ -> 0)
            in
            results := {
              base_offset;
              size;
              element_size = 4;
            } :: !results
          | _ -> ()
        end
      | _ -> ()
    ) bb.stmts
  ) target_f.entry target_f.cfg;

  !results

(* Step 4: virtual stack shape 구성 *)
let build_stack_shape
  (var_set : var_set)
  (arrays : array_info list)
  : stack_slot list =

  (* array base_offset set *)
  let array_bases = List.map (fun a -> a.base_offset) arrays in

  (* var_set의 각 주소를 슬롯으로 변환 *)
  let var_slots = List.filter_map (fun (addr_str, _) ->
    try
      let addr = int_of_string addr_str in
      let offset = addr - 100000 in  (* %arg_esp 기준 오프셋 *)
      if List.mem offset array_bases then
        (* 이 주소는 배열 base -> array slot으로 처리 *)
        let arr = List.find (fun a -> a.base_offset = offset) arrays in
        Some {
          start_offset = offset;
          end_offset = offset + (arr.size * arr.element_size);
          entry = Array arr;
        }
      else
        (* 단순 변수 *)
        Some {
          start_offset = offset;
          end_offset = offset + 4;
          entry = Variable;
        }
    with _ -> None
  ) var_set in

  (* offset 기준 정렬 (음수니까 내림차순 = 스택 top부터) *)
  List.sort (fun a b -> compare b.start_offset a.start_offset) var_slots

(* 결과 출력 *)
let pp_stack_shape (slots : stack_slot list) (var_set : var_set) =
  Format.printf "=== Virtual Stack Shape ===@.";
  List.iter (fun slot ->
    let addr_val = slot.start_offset + 100000 in
    let offset_str = 
      if addr_val >= 100000 then
        Printf.sprintf "0x%x" (addr_val - 100000)
      else
        Printf.sprintf "#x%lx" (Int32.of_int (addr_val - 100000))
    in
    match slot.entry with
    | Variable ->
      Format.printf "  [%d ~ %d] : %s@."
        slot.end_offset slot.start_offset offset_str
    | Array arr ->
      Format.printf "  [%d ~ %d] : array[%d] (%s)@."
        slot.end_offset slot.start_offset arr.size offset_str
  ) slots

let pp_json (slots : stack_slot list) (oc : out_channel) =
  let fmt = Format.formatter_of_out_channel oc in
  let variables = List.filter (fun s -> s.entry = Variable) slots in
  let arrays = List.filter (fun s -> match s.entry with Array _ -> true | _ -> false) slots in

  let offset_str start =
    let addr_val = start + 100000 in
    if addr_val >= 100000 then
      Printf.sprintf "0x%x" (addr_val - 100000)
    else
      Printf.sprintf "#x%lx" (Int32.of_int (addr_val - 100000))
  in

  Format.fprintf fmt "{@\n";

  (* variables *)
  Format.fprintf fmt "  \"variables\": [@\n";
  List.iteri (fun i slot ->
    Format.fprintf fmt "    {\"offset\": \"%s\", \"start\": %d, \"end\": %d}"
      (offset_str slot.start_offset)
      slot.start_offset
      slot.end_offset;
    if i < List.length variables - 1 then Format.fprintf fmt ",";
    Format.fprintf fmt "@\n"
  ) variables;
  Format.fprintf fmt "  ],@\n";

  (* arrays *)
  Format.fprintf fmt "  \"arrays\": [@\n";
  List.iteri (fun i slot ->
    match slot.entry with
    | Array arr ->
      Format.fprintf fmt "    {\"offset\": \"%s\", \"start\": %d, \"end\": %d, \"size\": %d, \"element_size\": %d}"
        (offset_str slot.start_offset)
        slot.start_offset
        slot.end_offset
        arr.size
        arr.element_size;
      if i < List.length arrays - 1 then Format.fprintf fmt ",";
      Format.fprintf fmt "@\n"
    | _ -> ()
  ) arrays;
  Format.fprintf fmt "  ]@\n";

  Format.fprintf fmt "}@\n";
  Format.pp_print_flush fmt ()
