open Calli

module AbsValue = AbsValue

(* ===== 타입 정의 ===== *)

(* 배열 정보를 나타내는 레코드.
    스택에서 감지된 배열의 위치와 크기 정보를 담는다. *)
type array_info = {
  base_offset : int;   (* %arg_esp 기준 오프셋, ex: -48 *)
  size : int;          (* 배열 원소 개수 *)
  element_size : int;  (* 원소 크기, 4 고정 (32비트 int 기준) *)
}

(* 스택 엔트리 종류.
    단순 변수이거나 배열이거나 둘 중 하나. *)
type stack_entry =
  | Variable          (* 단순 스칼라 변수 *)
  | Array of array_info  (* 배열 (메타 정보 포함) *)

(* 스택의 특정 슬롯을 나타내는 레코드.
    시작/끝 오프셋과 해당 슬롯의 종류를 담는다. *)
type stack_slot = {
  start_offset : int;  (* 슬롯 시작 오프셋 (낮은 주소) *)
  end_offset : int;    (* 슬롯 끝 오프셋 (높은 주소) *)
  entry : stack_entry; (* 슬롯 종류 (변수 or 배열) *)
}

(* 변수 집합: 주소값 문자열 (ex: "99952") -> 해당 주소에 매핑된 변수 이름 리스트.
    group_by_value_with_env 함수의 결과를 그대로 활용한다. *)
type var_set = (string * string list) list


(* [merge_overlapping_arrays arrays]
    탐지된 배열 리스트에서 offset 범위가 겹치는 배열들을 하나로 합친다.
    
    겹침 판단 기준:
      배열 A의 범위: [A.base_offset, A.base_offset + A.size * A.element_size)
      배열 B의 범위: [B.base_offset, B.base_offset + B.size * B.element_size)
      두 범위가 겹치면 -> 하나로 합침 (start = min, end = max)
      겹치지 않으면 -> 각각 별개 배열로 유지 *)
      let merge_overlapping_arrays (arrays : array_info list) : array_info list =

        (* 배열의 끝 offset 계산 헬퍼 *)
        let end_of a = a.base_offset + a.size * a.element_size in
      
        (* 두 배열의 범위가 겹치는지 확인
            A의 시작이 B의 끝보다 작고, B의 시작이 A의 끝보다 작으면 겹침 *)
        let overlaps a b =
          a.base_offset < end_of b && b.base_offset < end_of a
        in
      
        (* 두 배열을 하나로 합침
            새로운 base_offset = min(a.base_offset, b.base_offset)
            새로운 end_offset = max(end_of a, end_of b)
            size = (new_end - new_base) / element_size *)
        let merge_two a b =
          let new_base = min a.base_offset b.base_offset in
          let new_end = max (end_of a) (end_of b) in
          let new_size = (new_end - new_base) / a.element_size in
          { base_offset = new_base; size = new_size; element_size = a.element_size }
        in
      
        (* 하나의 배열을 기존 누적 리스트에 병합하거나 새로 추가
            겹치는 배열이 있으면 합치고, 없으면 그대로 추가 *)
        let merge_into acc arr =
          let merged = ref false in
          let result = List.map (fun existing ->
            if overlaps existing arr then begin
              merged := true;
              merge_two existing arr  (* 겹치면 두 배열을 하나로 합침 *)
            end else
              existing  (* 안 겹치면 그대로 유지 *)
          ) acc in
          (* 겹치는 배열이 없었으면 새 배열로 추가 *)
          if !merged then result
          else acc @ [arr]
        in
      
        (* 연쇄 겹침(A-B, B-C처럼 A와 C가 B를 통해 연결되는 경우)을 처리하기 위해
            변경이 없을 때까지 반복 수행 *)
        let rec fix_point arrays =
          let merged = List.fold_left merge_into [] arrays in
          (* 병합 전후 길이가 같으면 더 이상 합칠 배열이 없음 -> 종료 *)
          if List.length merged = List.length arrays then merged
          else fix_point merged
        in
      
        fix_point arrays


(* ===== Step 3: add 명령어 순회를 통한 배열 탐지 ===== *)
(* [detect_arrays target_f var_set s2_exit_mem]
    함수 [target_f]의 CFG를 순회하며 BinaryOp(Add) 명령어를 분석해
    배열로 추정되는 스택 변수를 탐지한다.

    - [var_set]: 변수 이름 -> 스택 주소 매핑
    - [s2_exit_mem]: Step 2에서 분석된 추상 메모리 (interval 값 포함)

    탐지 기준:
      base_name이 var_set에 속하고,
      idx_expr의 추상 값이 singleton이 아닌 IntInterval일 때
      -> 배열로 간주하고 array_info를 생성한다. *)
let detect_arrays 
  (target_f : Function.t)
  (var_set : var_set)
  (s2_exit_mem : AbsMemory.t)
  : array_info list =

  (* var_set에서 변수 이름 -> 주소 문자열 역방향 매핑 테이블 구성 *)
  let var_names : (string, string) Hashtbl.t = Hashtbl.create 100 in
  List.iter (fun (addr_str, names) ->
    List.iter (fun name -> Hashtbl.replace var_names name addr_str) names
  ) var_set;

  (* 탐지된 배열 결과 누적 리스트 *)
  let results = ref [] in

  (* [try_detect base_name idx_expr]
      base_name이 var_set 소속 변수이고,
      idx_expr의 추상 interval이 유효한 범위를 가질 때
      array_info를 생성해 results에 추가한다. *)
  let try_detect base_name idx_expr =
    if Hashtbl.mem var_names base_name then begin
      (* idx_expr이 Name이면 추상 메모리에서 interval 값 조회 *)
      let idx_absval = match idx_expr with
        | Expr.Name {name; _} ->
          (try
            let addr = Env.find name !Env.env in
            Some (AbsMemory.find addr s2_exit_mem)
          with _ -> None)
        | _ -> None
      in
      match idx_absval with
      (* singleton이 아닌 정수 interval인 경우에만 배열로 판단 *)
      | Some (AbsValue.AbsInt (AbsInterval.IntInterval {
          min = AbsInterval.I lo; 
          max = AbsInterval.I hi})) 
        when not (Z.equal lo hi) ->
        (* range = hi - lo, 원소 크기(4)로 나눠 배열 크기 계산 *)
        let hi_int = Z.to_int hi in
        let size = hi_int / 4 + 1 in
        let base_addr_str = Hashtbl.find var_names base_name in
        let base_offset = 
          (try int_of_string base_addr_str - 100000
          with _ -> 0)
        in
        let existing = List.find_opt (fun a -> a.base_offset = base_offset) !results in
        (match existing with
        | Some _ ->
          results := List.map (fun a ->
            if a.base_offset = base_offset then
              { a with size = max a.size size }
            else a
          ) !results
        | None ->
          results := {
            base_offset;
            size;
            element_size = 4;
          } :: !results)
      | _ -> ()
    end
  in

  (* CFG를 entry부터 순회하며 BinaryOp(Add) 명령어를 찾는다 *)
  Cfg.iter_from_entry (fun bb_name ->
    let bb = Bbpool.find bb_name !Bbpool.pool in
    List.iter (fun (stmt : Stmt.t) ->
      match stmt.inst with
      | BinaryOp {op = Add; operand0; operand1; _} ->
        (* 케이스 1: operand0 = base 변수, operand1 = 인덱스 *)
        (match operand0 with
        | Expr.Name {name; _} -> try_detect name operand1
        | _ -> ());
        (* 케이스 2: operand1 = base 변수, operand0 = 인덱스 (교환법칙 대응) *)
        (match operand1 with
        | Expr.Name {name; _} -> try_detect name operand0
        | _ -> ())
      | _ -> ()
    ) bb.stmts
  ) target_f.entry target_f.cfg;
!results


(* ===== Step 4: 가상 스택 형태(stack shape) 구성 ===== *)

(* [build_stack_shape var_set arrays]
    var_set의 각 주소를 슬롯으로 변환하고,
    배열 base 주소인 경우 Array 슬롯으로, 아닌 경우 Variable 슬롯으로 분류한다.
    결과는 start_offset 내림차순(스택 top 방향)으로 정렬된다. *)
let build_stack_shape
  (var_set : var_set)
  (arrays : array_info list)
  : stack_slot list =

  (* 탐지된 배열들의 base_offset 목록 *)
  let array_bases = List.map (fun a -> a.base_offset) arrays in

  (* 주어진 offset이 어떤 배열의 내부 원소 범위에 속하는지 확인 *)
  let in_array_range offset =
    List.exists (fun a ->
      offset > a.base_offset && offset < a.base_offset + (a.size * a.element_size)
    ) arrays
  in

  (* var_set의 각 항목을 stack_slot으로 변환 *)
  let var_slots = List.filter_map (fun (addr_str, _) ->
    try
      let addr = int_of_string addr_str in
      let offset = addr - 100000 in  (* 절대 주소 -> %arg_esp 기준 오프셋 *)
      if List.mem offset array_bases then
        let arr = List.find (fun a -> a.base_offset = offset) arrays in
        Some {
          start_offset = offset;
          end_offset = offset + (arr.size * arr.element_size);
          entry = Array arr;
        }
      else if in_array_range offset then
        None  (* 배열 범위 안 -> 스킵 *)
      else
        Some {
          start_offset = offset;
          end_offset = offset + 4;
          entry = Variable;
        }
    with _ -> None
  ) var_set in

  (* start_offset 내림차순 정렬: 음수 오프셋이므로 내림차순 = 스택 top부터 *)
  List.sort (fun a b -> compare b.start_offset a.start_offset) var_slots


(* [merge_array_vars var_set arrays]
    var_set에서 배열의 내부 원소에 해당하는 주소들을
    해당 배열의 base_offset 항목으로 병합한다.

    예: arr[0], arr[1], arr[2]가 별도 항목으로 있으면
        모두 arr 배열의 base 항목에 통합된다. *)
let merge_array_vars
  (var_set : var_set)
  (arrays : array_info list)
  : var_set =

  (* 주어진 offset이 어떤 배열의 내부 범위에 속하면 그 배열의 base_offset 반환 *)
  let find_array_base offset =
    List.find_opt (fun a ->
      offset > a.base_offset && offset < a.base_offset + (a.size * a.element_size)
    ) arrays
    |> Option.map (fun a -> a.base_offset)
  in

  (* base_offset -> 변수 이름 리스트를 누적하는 해시테이블 *)
  let merged : (int, string list) Hashtbl.t = Hashtbl.create 100 in

  (* 모든 var_set 항목을 순회하며 배열 범위 내 주소는 base로 병합 *)
  List.iter (fun (addr_str, names) ->
    try
      let addr = int_of_string addr_str in
      let offset = addr - 100000 in
      (* 배열 범위 안이면 base로 병합, 아니면 자기 자신 *)
      let target_offset = match find_array_base offset with
        | Some base -> base
        | None -> offset
      in
      let prev = try Hashtbl.find merged target_offset with Not_found -> [] in
      Hashtbl.replace merged target_offset (prev @ names)
    with _ -> ()
  ) var_set;

  (* Hashtbl -> var_set (string * string list) list 형태로 변환 *)
  Hashtbl.fold (fun offset names acc ->
    let addr_str = string_of_int (offset + 100000) in
    (addr_str, names) :: acc
  ) merged []


(* ===== 출력 함수들 ===== *)

(* [pp_stack_shape slots var_set]
    스택 슬롯 목록을 사람이 읽기 좋은 형태로 출력한다.
    변수는 단순 범위로, 배열은 크기 정보와 함께 출력한다. *)
let pp_stack_shape (slots : stack_slot list) (var_set : var_set) =
  Format.printf "=== Virtual Stack Shape ===@.";
  List.iter (fun slot ->
    (* 오프셋을 16진수 문자열로 변환 (음수 오프셋 처리 포함) *)
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

(* [pp_json slots oc]
    스택 슬롯 목록을 JSON 형식으로 [oc] 채널에 출력한다.
    "variables"와 "arrays" 두 키로 분리하여 출력한다. *)
let pp_json (slots : stack_slot list) (oc : out_channel) =
  let fmt = Format.formatter_of_out_channel oc in
  (* 변수 슬롯과 배열 슬롯을 분리 *)
  let variables = List.filter (fun s -> s.entry = Variable) slots in
  let arrays = List.filter (fun s -> match s.entry with Array _ -> true | _ -> false) slots in

  (* start_offset -> 16진수 오프셋 문자열 변환 헬퍼 *)
  let offset_str start =
    let addr_val = start + 100000 in
    if addr_val >= 100000 then
      Printf.sprintf "0x%x" (addr_val - 100000)
    else
      Printf.sprintf "#x%lx" (Int32.of_int (addr_val - 100000))
  in

  Format.fprintf fmt "{@\n";

  (* variables 배열 출력 *)
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

  (* arrays 배열 출력 (size, element_size 포함) *)
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

(* [pp_var_set_json var_set oc]
    var_set (주소 -> 변수 이름 리스트)을 JSON 형식으로 출력한다.
    변수 이름은 '%' 이후 부분만 추출하고,
    주소는 %arg_esp 기준 오프셋 16진수로 변환하여 출력한다. *)
let pp_var_set_json (var_set : var_set) (oc : out_channel) =
  let fmt = Format.formatter_of_out_channel oc in

  (* 변수 이름에서 '%' 이후 부분만 추출 (SSA 이름 형식 처리) *)
  let extract_var_name key =
    try
      let idx = String.rindex key '%' in
      Some (String.sub key idx (String.length key - idx))
    with Not_found -> None
  in

  (* 절대 주소 문자열 -> %arg_esp 기준 오프셋 16진수 문자열 변환 *)
  let transform_offset addr_str =
    try
      let addr = int_of_string addr_str in
      let offset = addr - 100000 in
      if offset >= 0 then
        Printf.sprintf "0x%x" offset
      else
        Printf.sprintf "#x%lx" (Int32.of_int offset)
    with _ -> addr_str
  in

  Format.fprintf fmt "{@[<v>";
  List.iteri (fun i (addr_str, names) ->
    (* '%' 포함 이름만 유효한 변수로 처리 *)
    let valid_names = List.filter_map extract_var_name names in
    if valid_names <> [] then begin
      Format.fprintf fmt "@,  \"%s\": [@[<v>" (transform_offset addr_str);
      List.iteri (fun j name ->
        Format.fprintf fmt "@,    {\"var\": \"%s\"}" name;
        if j < List.length valid_names - 1 then Format.fprintf fmt ","
      ) valid_names;
      Format.fprintf fmt "@]@,  ]";
      if i < List.length var_set - 1 then Format.fprintf fmt ","
    end
  ) var_set;
  Format.fprintf fmt "@]@,}";
  Format.pp_print_flush fmt ()