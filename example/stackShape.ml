open Calli

module AbsValue = AbsValue

(* ===== 타입 정의 ===== *)

(* 스택 엔트리 종류.
   - Variable: 단독 스칼라 변수.
   - Chunk: 덩어리. 범위(start/end)만 갖는다. 배열/struct 같은 종류나
            원소 타입은 여기서 판정하지 않는다(타입은 ELIS type graph에서 정한다). *)
type stack_entry =
  | Variable
  | Chunk

(* 스택의 특정 슬롯 *)
type stack_slot = {
  start_offset : int;  (* 슬롯 시작 오프셋 (낮은 주소) *)
  end_offset : int;    (* 슬롯 끝 오프셋 (높은 주소, inclusive) *)
  entry : stack_entry;
}

(* 변수 집합: 주소값 문자열 (ex: "99952") -> 변수 이름 리스트 *)
type var_set = (string * string list) list


(* ===== 공통 헬퍼 ===== *)

(* %arg_esp에 대응되는 가상 base 주소 *)
let base_addr = 100000

(* [addr_to_offset addr_str]
   절대 주소 문자열을 %arg_esp 기준 오프셋(int)으로 변환한다.
   파싱 실패 시 None. *)
let addr_to_offset (addr_str : string) : int option =
  try Some (int_of_string addr_str - base_addr)
  with _ -> None

(* [offset_to_hex offset]
   오프셋을 16진수 문자열로 변환한다.
   음수 오프셋은 32비트 2의 보수 표현에 #x 접두어를 붙인다. *)
let offset_to_hex (offset : int) : string =
  if offset >= 0 then Printf.sprintf "0x%x" offset
  else Printf.sprintf "#x%lx" (Int32.of_int offset)

(* [build_var_names var_set]
   var_set(주소 -> 이름 리스트)으로부터
   변수 이름 -> 주소 문자열 역방향 매핑 테이블을 구성한다. *)
let build_var_names (var_set : var_set) : (string, string) Hashtbl.t =
  let tbl = Hashtbl.create 100 in
  List.iter (fun (addr_str, names) ->
    List.iter (fun name -> Hashtbl.replace tbl name addr_str) names
  ) var_set;
  tbl

(* [interval_hi_of_expr idx_expr mem]
   idx_expr가 Name이고 그 추상값이 singleton이 아닌 IntInterval이면
   상한(hi)을 int로 반환, 아니면 None. *)
let interval_hi_of_expr (idx_expr : Expr.t) (mem : AbsMemory.t) : int option =
  match idx_expr with
  | Expr.Name {name; _} ->
    (try
      let addr = Env.find name !Env.env in
      match AbsMemory.find addr mem with
      | AbsValue.AbsInt (AbsInterval.IntInterval {
          min = AbsInterval.I lo;
          max = AbsInterval.I hi})
        when not (Z.equal lo hi) -> Some (Z.to_int hi)
      | _ -> None
    with _ -> None)
  | _ -> None

(* [iter_array_accesses target_f var_set s2_exit_mem f]
   CFG를 entry부터 순회하며 BinaryOp(Add) 명령어에서
   base 변수 + non-singleton interval 인덱스 패턴을 찾을 때마다
   [f base_offset hi]를 호출한다.
   detect_arrays / detect_loop_chunks의 공통 골격. *)
let iter_array_accesses
    (target_f : Function.t)
    (var_set : var_set)
    (s2_exit_mem : AbsMemory.t)
    (f : int -> int -> unit) : unit =

  let var_names = build_var_names var_set in

  let try_detect base_name idx_expr =
    match Hashtbl.find_opt var_names base_name with
    | None -> ()
    | Some base_addr_str ->
      (match interval_hi_of_expr idx_expr s2_exit_mem with
       | Some hi ->
         let base_offset =
           match addr_to_offset base_addr_str with
           | Some o -> o
           | None -> 0
         in
         f base_offset hi
       | None -> ())
  in

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
  ) target_f.entry target_f.cfg

(* [merge_by ~overlaps ~merge_two items]
   겹침 판단 함수와 병합 함수를 받아 fixpoint 방식으로 병합하는 범용 함수.
   연쇄 겹침(A-B, B-C처럼 A와 C가 B를 통해 연결)도 처리한다.
   merge_overlapping_arrays / merge_chunks의 공통 골격. *)
let merge_by
    ~(overlaps : 'a -> 'a -> bool)
    ~(merge_two : 'a -> 'a -> 'a)
    (items : 'a list) : 'a list =

  (* 하나의 항목을 누적 리스트에 병합하거나 새로 추가 *)
  let merge_into acc item =
    let merged = ref false in
    let result = List.map (fun existing ->
      if overlaps existing item then begin
        merged := true;
        merge_two existing item
      end else
        existing
    ) acc in
    if !merged then result
    else acc @ [item]
  in

  (* 변화가 없을 때까지 반복 *)
  let rec fix_point items =
    let merged = List.fold_left merge_into [] items in
    if List.length merged = List.length items then merged
    else fix_point merged
  in
  fix_point items

(* [detect_loop_chunks target_f var_set s2_exit_mem]
   탐지 조건은 detect_arrays와 같지만, size 계산 없이
   (base_offset, base_offset + hi) 범위 리스트만 반환한다.
   같은 (start, end) 쌍은 중복 없이 한 번만 기록된다. *)
let detect_loop_chunks
    (target_f : Function.t)
    (var_set : var_set)
    (s2_exit_mem : AbsMemory.t)
    : (int * int) list =

  let results = ref [] in
  iter_array_accesses target_f var_set s2_exit_mem (fun base_offset hi ->
    let chunk = (base_offset, base_offset + hi) in
    if not (List.mem chunk !results) then
      results := chunk :: !results
  );
  !results


(* [extend_loop_chunk_ends loop_chunks var_set other_chunks]
   loop 기반 chunk는 인덱스 접근 범위(base ~ base+hi)까지만 근거가 있고,
   base+hi는 마지막 원소가 "시작"하는 offset이다. 원소 폭(타입)은 쓰지 않고,
   end를 그 위쪽에서 처음 나오는 offset(var_set 변수, 다른 chunk의 시작,
   없으면 0 = %arg_esp)의 바로 앞까지 늘린다.
   padding이나 사이에 있는 미탐지 객체가 함께 잡힐 수 있으나 감수한다.
   memset/memcpy/라이브러리 chunk는 크기를 정확히 알므로 대상이 아니다. *)
let extend_loop_chunk_ends
    (loop_chunks : (int * int) list)
    (var_set : var_set)
    (other_chunks : (int * int) list)
    : (int * int) list =
  let boundaries =
    0
    :: List.map fst other_chunks
    @ List.map fst loop_chunks
    @ List.filter_map (fun (addr_str, _) -> addr_to_offset addr_str) var_set
  in
  List.map (fun (s, e) ->
    match List.filter (fun o -> o > e) boundaries with
    | [] -> (s, e)
    | above -> (s, List.fold_left min max_int above - 1)
  ) loop_chunks


(* ===== 병합 함수들 ===== *)

(* [merge_chunks chunks]
   (start, end) 범위 리스트에서 실제로 포개지는 것들만 병합한다.
   '<' 비교이므로 딱 맞닿기(a_end = b_start)는 병합하지 않는다(unsound 방지). *)
let merge_chunks (chunks : (int * int) list) : (int * int) list =
  merge_by
    ~overlaps:(fun (a_s, a_e) (b_s, b_e) -> a_s < b_e && b_s < a_e)
    ~merge_two:(fun (a_s, a_e) (b_s, b_e) -> (min a_s b_s, max a_e b_e))
    chunks

(* [build_stack_shape var_set chunks]
   var_set과 chunks(병합된 덩어리 범위)로부터 전체 stack_slot 리스트를 구성한다.
   - chunks 범위는 Chunk 슬롯으로.
   - chunks 어디에도 속하지 않는 var_set 변수는 Variable 슬롯으로.
   결과는 start_offset 내림차순(스택 위쪽 -> 아래쪽)으로 정렬해 반환한다. *)
let build_stack_shape
    (var_set : var_set)
    (chunks : (int * int) list)
    : stack_slot list =

  let in_any_chunk offset =
    List.exists (fun (s, e) -> s <= offset && offset <= e) chunks
  in

  let chunk_slots =
    List.map (fun (s, e) ->
      { start_offset = s; end_offset = e; entry = Chunk }
    ) chunks
  in

  let var_slots =
    List.filter_map (fun (addr_str, _) ->
      match addr_to_offset addr_str with
      | None -> None
      | Some offset ->
        if in_any_chunk offset then None
        else
          Some { start_offset = offset;
                 end_offset = offset + 4;
                 entry = Variable }
    ) var_set
  in

  List.sort (fun a b -> compare b.start_offset a.start_offset)
    (chunk_slots @ var_slots)

(* [add_libraryfunc_chunks modeled_buffers]
   전처리(라이브러리 함수 모델링)가 알려준 버퍼
   (start_offset, byte_length, source_function)를
   [start, start + byte_length - 1] (inclusive) chunk로 반환한다.
   크기를 정확히 알 수 있으므로 end를 그대로 쓴다.
   반환된 chunk들은 다른 chunk들과 함께 merge_chunks로 병합한다. *)
let add_libraryfunc_chunks
    (modeled_buffers : (int * int * string) list)
    : (int * int) list =
  List.filter_map (fun (start, length, _source) ->
    if length <= 0 then None
    else Some (start, start + length - 1)
  ) modeled_buffers

(* ===== 출력 함수들 ===== *)

(* [pp_stack_shape slots var_set]
   스택 슬롯 목록을 사람이 읽기 좋은 형태로 출력한다.
   Variable은 "[start] : #x주소", Chunk는 "[end ~ start] : chunk (#x주소)".
   chunk의 end_offset은 내부적으로 inclusive이므로, 출력 시에만
   +1(exclusive)로 보여준다. *)
let pp_stack_shape (slots : stack_slot list) (_var_set : var_set) =
  Format.printf "=== Virtual Stack Shape ===@.";
  List.iter (fun slot ->
    let offset_str = offset_to_hex slot.start_offset in
    match slot.entry with
    | Variable ->
      Format.printf "  [%d] : %s@." slot.start_offset offset_str
    | Chunk ->
      Format.printf "  [%d ~ %d] : chunk (%s)@."
        (slot.end_offset + 1) slot.start_offset offset_str
  ) slots

(* [pp_json slots oc]
   스택 슬롯 목록을 "variables" / "chunks" 두 키로 분리해 JSON 형태로
   out_channel에 출력한다.
   - variables 항목은 offset/start만 담는다. 단독 스칼라 변수는 크기 근거가
     없으므로 end를 출력하지 않는다.
   - chunks 항목은 offset/start/end만 담는다. end는 내부 inclusive 값에
     +1 해서 exclusive로 출력한다. *)
let pp_json (slots : stack_slot list) (oc : out_channel) =
  let fmt = Format.formatter_of_out_channel oc in

  let variables = List.filter (fun s -> s.entry = Variable) slots in
  let chunks = List.filter (fun s -> s.entry = Chunk) slots in

  let print_list ~last key items =
    Format.fprintf fmt "  \"%s\": [@\n" key;
    let n = List.length items in
    List.iteri (fun i slot ->
      Format.fprintf fmt "    {@\n";
      (match slot.entry with
       | Variable ->
         Format.fprintf fmt "      \"offset\": \"%s\", \"start\": %d@\n"
           (offset_to_hex slot.start_offset)
           slot.start_offset
       | Chunk ->
         Format.fprintf fmt "      \"offset\": \"%s\", \"start\": %d, \"end\": %d@\n"
           (offset_to_hex slot.start_offset)
           slot.start_offset
           (slot.end_offset + 1));
      Format.fprintf fmt "    }";
      if i < n - 1 then Format.fprintf fmt ",";
      Format.fprintf fmt "@\n"
    ) items;
    if last then Format.fprintf fmt "  ]@\n"
    else Format.fprintf fmt "  ],@\n"
  in

  Format.fprintf fmt "{@\n";
  print_list ~last:false "variables" variables;
  print_list ~last:true  "chunks" chunks;
  Format.fprintf fmt "}@\n";
  Format.pp_print_flush fmt ()


(* [pp_var_set_json var_set oc]
   var_set을 JSON으로 출력한다(output.json 산출용).
   변수 이름은 '%' 이후만 추출, 주소는 오프셋 16진수로 변환한다. *)
let pp_var_set_json (var_set : var_set) (oc : out_channel) =
  let fmt = Format.formatter_of_out_channel oc in

  (* 변수 이름에서 '%' 이후 부분만 추출 (SSA 이름 형식 처리) *)
  let extract_var_name key =
    try
      let idx = String.rindex key '%' in
      Some (String.sub key idx (String.length key - idx))
    with Not_found -> None
  in

  let transform_offset addr_str =
    match addr_to_offset addr_str with
    | Some offset -> offset_to_hex offset
    | None -> addr_str
  in

  (* 유효한 이름이 있는 항목만 미리 골라내서 콤마 위치가 어긋나지 않게 한다 *)
  let entries =
    List.filter_map (fun (addr_str, names) ->
      match List.filter_map extract_var_name names with
      | [] -> None
      | valid_names -> Some (addr_str, valid_names)
    ) var_set
  in

  Format.fprintf fmt "{@[<v>";
  let n = List.length entries in
  List.iteri (fun i (addr_str, valid_names) ->
    Format.fprintf fmt "@,  \"%s\": [@[<v>" (transform_offset addr_str);
    let m = List.length valid_names in
    List.iteri (fun j name ->
      Format.fprintf fmt "@,    {\"var\": \"%s\"}" name;
      if j < m - 1 then Format.fprintf fmt ","
    ) valid_names;
    Format.fprintf fmt "@]@,  ]";
    if i < n - 1 then Format.fprintf fmt ","
  ) entries;
  Format.fprintf fmt "@]@,}";
  Format.pp_print_flush fmt ()