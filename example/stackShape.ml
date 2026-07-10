open Calli

module AbsValue = AbsValue
module IntSet = Set.Make(Int)

(* ===== 타입 정의 ===== *)

(* 덩어리의 해석된 종류. 지금 단계에서는 전부 Unknown으로 생성되고,
   나중에 타입 매핑 단계에서 ArrayKind / StructKind로 채워진다. *)
type chunk_kind =
  | Unknown
  | ArrayKind
  | StructKind

(* 스택 엔트리 종류.
   - Variable: 단독 스칼라 변수. 타입을 따지지 않는다.
   - Chunk: 배열/struct 후보 덩어리. 종류(kind)만 갖고,
            범위(start/end)는 stack_slot이 표현한다. *)
type stack_entry =
  | Variable
  | Chunk of chunk_kind

(* 스택의 특정 슬롯 *)
type stack_slot = {
  start_offset : int;  (* 슬롯 시작 오프셋 (낮은 주소) *)
  end_offset : int;    (* 슬롯 끝 오프셋 (높은 주소) *)
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

(* [target_offsets ptr_val]
   포인터 값(AbsAddr)이 가리키는 모든 주소를 offset 리스트로 꺼낸다.
   AddrSet의 각 주소를 offset으로 변환하고, 변환 실패한 건 뺀다.
   주소가 아니면(AbsAddr가 아니면) 빈 리스트. *)
let target_offsets (ptr_val : AbsValue.t) : int list =
  match ptr_val with
  | AbsValue.AbsAddr a ->
    AbsValue.AbsAddr.fold (fun addr acc ->
      match addr_to_offset addr with
      | Some off -> off :: acc
      | None -> acc
    ) a []
  | _ -> []

(* [store_target_offsets name mem]
   store의 name(포인터 변수 이름)이 가리키는 offset들을 반환한다.
   Env/메모리 조회 실패 시 빈 리스트. *)
let store_target_offsets (name : string) (mem : AbsMemory.t) : int list =
  try
    let addr = Env.find name !Env.env in
    target_offsets (AbsMemory.find addr mem)
  with _ -> []

(* [load_target_offsets operand mem]
   load의 operand(포인터 표현식)가 가리키는 offset들을 반환한다.
   operand가 Name이 아니거나 조회 실패 시 빈 리스트. *)
let load_target_offsets (operand : Expr.t) (mem : AbsMemory.t) : int list =
  match operand with
  | Expr.Name {name; _} ->
    (try target_offsets (AbsMemory.find (Env.find name !Env.env) mem)
     with _ -> [])
  | _ -> []

(* [byte_width_of_type ty]
   Type.t에서 바이트 폭을 꺼낸다. Integer bitwidth를 8로 나눈다.
   그 외 타입은 None. *)
let byte_width_of_type (ty : Type.t) : int option =
  match ty with
  | Type.Integer {bitwidth} -> Some ((if bitwidth < 8 then 8 else bitwidth) / 8)
  | _ -> None

(* [byte_width_of_ptr_type ty]
   Pointer 타입의 대상 타입에서 바이트 폭을 꺼낸다.
   ex: i8* -> 1, i32* -> 4. Pointer가 아니거나 대상이 Integer가 아니면 None. *)
let byte_width_of_ptr_type (ty : Type.t) : int option =
  match ty with
  | Type.Pointer {ty = inner} -> byte_width_of_type inner
  | _ -> None

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

(* [collect_array_widths target_f var_set s2_exit_mem]
   배열 접근 add(base + idx) -> inttoptr -> load/store 패턴을 찾아,
   그 결과를 받는 inttoptr의 목적 타입에서 폭을 뽑아
   base ~ base+hi 범위 전체(원소 폭만큼)에 기록한다.
   collect_offset_types와는 별도의 테이블을 반환하며,
   loop처럼 인덱스로 접근하는 "진짜 원소 단위" 폭 정보를 담는다. *)
let collect_array_widths
    (target_f : Function.t)
    (var_set : var_set)
    (s2_exit_mem : AbsMemory.t)
    : (int, IntSet.t) Hashtbl.t =

  let tbl : (int, IntSet.t) Hashtbl.t = Hashtbl.create 50 in
  let var_names = build_var_names var_set in

  let add offset width =
    let prev = Option.value (Hashtbl.find_opt tbl offset) ~default:IntSet.empty in
    Hashtbl.replace tbl offset (IntSet.add width prev)
  in

  Cfg.iter_from_entry (fun bb_name ->
    let bb = Bbpool.find bb_name !Bbpool.pool in

    (* 이 블록의 inttoptr 목록: (operand 이름 -> 폭) 매핑 미리 구성 *)
    let ptr_widths : (string, int) Hashtbl.t = Hashtbl.create 20 in
    List.iter (fun (stmt : Stmt.t) ->
      match stmt.inst with
      | IntToPtr {operand = Expr.Name {name = opname; _}; ty; _} ->
        (match byte_width_of_ptr_type ty with
         | Some w -> Hashtbl.replace ptr_widths opname w
         | None -> ())
      | _ -> ()
    ) bb.stmts;

    (* add(base + idx) 배열 접근을 찾아, 그 결과를 받는 inttoptr 폭을 기록 *)
    let handle add_name base_name idx_expr =
      match Hashtbl.find_opt var_names base_name with
      | None -> ()
      | Some base_addr_str ->
        (match interval_hi_of_expr idx_expr s2_exit_mem with
         | Some hi ->
           (match addr_to_offset base_addr_str, Hashtbl.find_opt ptr_widths add_name with
            | Some base_offset, Some width ->
              (* base부터 base+hi까지, 원소 폭(width)만큼씩 채워가며 기록 *)
              let i = ref 0 in
              while !i <= hi do
                for b = 0 to width - 1 do
                  add (base_offset + !i + b) width
                done;
                i := !i + width
              done
            | _ -> ())
         | None -> ())
    in

    List.iter (fun (stmt : Stmt.t) ->
      match stmt.inst with
      | BinaryOp {name; op = Add; operand0; operand1; _} ->
        (match operand0 with
         | Expr.Name {name = n; _} -> handle name n operand1
         | _ -> ());
        (match operand1 with
         | Expr.Name {name = n; _} -> handle name n operand0
         | _ -> ())
      | _ -> ()
    ) bb.stmts
  ) target_f.entry target_f.cfg;
  tbl

(* [collect_offset_types target_f s2_exit_mem]
   store/load 명령을 순회하며 각 offset이 접근된 바이트 폭들을 모은다.
   (포인터 목적 타입 기반, byte_width_of_ptr_type 사용)
   반환: offset -> 폭들의 집합. *)
let collect_offset_types
    (target_f : Function.t)
    (s2_exit_mem : AbsMemory.t)
    : (int, IntSet.t) Hashtbl.t =

  let tbl : (int, IntSet.t) Hashtbl.t = Hashtbl.create 100 in

  let add offset width =
    let prev = Option.value (Hashtbl.find_opt tbl offset) ~default:IntSet.empty in
    Hashtbl.replace tbl offset (IntSet.add width prev)
  in

  Cfg.iter_from_entry (fun bb_name ->
    let bb = Bbpool.find bb_name !Bbpool.pool in
    List.iter (fun (stmt : Stmt.t) ->
      match stmt.inst with
      | Store {name; ty; _} ->
        (match byte_width_of_ptr_type ty with
         | Some width ->
           List.iter (fun off -> add off width) (store_target_offsets name s2_exit_mem)
         | None -> ())
      | Load {operand; ty; _} ->
        (match byte_width_of_ptr_type ty with
         | Some width ->
           List.iter (fun off -> add off width) (load_target_offsets operand s2_exit_mem)
         | None -> ())
      | _ -> ()
    ) bb.stmts
  ) target_f.entry target_f.cfg;
  tbl

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


(* ===== 병합 함수들 ===== *)

(* [merge_chunks chunks]
   (start, end) 범위 리스트에서 실제로 포개지는 것들만 병합한다.
   '<' 비교이므로 딱 맞닿기(a_end = b_start)는 병합하지 않는다(unsound 방지). *)
let merge_chunks (chunks : (int * int) list) : (int * int) list =
  merge_by
    ~overlaps:(fun (a_s, a_e) (b_s, b_e) -> a_s < b_e && b_s < a_e)
    ~merge_two:(fun (a_s, a_e) (b_s, b_e) -> (min a_s b_s, max a_e b_e))
    chunks

(* [merge_array_vars var_set chunks]
   var_set에서 어떤 chunk의 [s, e] 범위에 속하는 주소들을
   그 chunk의 시작 offset 항목으로 묶는다.
   chunk에 안 속하는 주소는 자기 자신으로 둔다.
   (output.json 산출용으로 계속 사용됨) *)
let merge_array_vars
    (var_set : var_set)
    (chunks : (int * int) list)
    : var_set =

  (* offset을 감싸는 chunk가 있으면 그 시작 offset을 반환 *)
  let containing_chunk_start offset =
    match List.find_opt (fun (s, e) -> s <= offset && offset <= e) chunks with
    | Some (s, _) -> Some s
    | None -> None
  in

  let merged : (int, string list) Hashtbl.t = Hashtbl.create 100 in

  List.iter (fun (addr_str, names) ->
    match addr_to_offset addr_str with
    | None -> ()
    | Some offset ->
      let target_offset =
        match containing_chunk_start offset with
        | Some s -> s
        | None -> offset
      in
      let prev = Option.value (Hashtbl.find_opt merged target_offset) ~default:[] in
      Hashtbl.replace merged target_offset (prev @ names)
  ) var_set;

  Hashtbl.fold (fun offset names acc ->
    (string_of_int (offset + base_addr), names) :: acc
  ) merged []

(* [determine_chunk_kind (s, e) offset_types array_widths]
   chunk 범위 [s, e]의 종류를 판정한다.
   1순위: array_widths(loop 등 indexed access 기반)가 범위 전체를
          균일한 폭 하나로 완전히 커버하면 곧바로 ArrayKind로 확정한다.
          (bulk 초기화 store 등으로 offset_types에 섞여드는
           잡음 폭을 걸러내기 위함)
   폴백: array_widths로 판정할 수 없으면 offset_types(store/load 폭)를
         모아 폭이 없으면 Unknown, 하나로 균일하면 ArrayKind,
         여러 개 섞이면 StructKind로 판정한다. *)
let determine_chunk_kind
    ((s, e) : int * int)
    (offset_types : (int, IntSet.t) Hashtbl.t)
    (array_widths : (int, IntSet.t) Hashtbl.t)
    : chunk_kind =

  let arr_widths = ref IntSet.empty in
  let fully_covered = ref true in
  for offset = s to e do
    match Hashtbl.find_opt array_widths offset with
    | Some ws -> arr_widths := IntSet.union !arr_widths ws
    | None -> fully_covered := false
  done;

  if !fully_covered && IntSet.cardinal !arr_widths = 1 then ArrayKind
  else begin
    let widths = ref IntSet.empty in
    for offset = s to e do
      match Hashtbl.find_opt offset_types offset with
      | Some ws -> widths := IntSet.union !widths ws
      | None -> ()
    done;
    match IntSet.cardinal !widths with
    | 0 -> Unknown
    | 1 -> ArrayKind
    | _ -> StructKind
  end

(* [build_stack_shape var_set chunks offset_types array_widths]
   var_set과 chunks(병합된 덩어리 범위)로부터 전체 stack_slot 리스트를 구성한다.
   - chunks 범위는 determine_chunk_kind로 kind를 판정해 Chunk 슬롯으로.
   - chunks 어디에도 속하지 않는 var_set 변수는 Variable 슬롯으로.
   결과는 start_offset 내림차순(스택 위쪽 -> 아래쪽)으로 정렬해 반환한다. *)
let build_stack_shape
    (var_set : var_set)
    (chunks : (int * int) list)
    (offset_types : (int, IntSet.t) Hashtbl.t)
    (array_widths : (int, IntSet.t) Hashtbl.t)
    : stack_slot list =

  let in_any_chunk offset =
    List.exists (fun (s, e) -> s <= offset && offset <= e) chunks
  in

  (* 1) chunk들을 판정된 kind로 Chunk 슬롯으로 *)
  let chunk_slots =
    List.map (fun (s, e) ->
      let kind = determine_chunk_kind (s, e) offset_types array_widths in
      { start_offset = s; end_offset = e; entry = Chunk kind }
    ) chunks
  in

  (* 2) 어떤 chunk에도 속하지 않는 var_set 변수만 Variable 슬롯으로 *)
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


(* ===== 출력 함수들 ===== *)

(* [pp_stack_shape slots var_set]
   스택 슬롯 목록을 사람이 읽기 좋은 형태로 출력한다.
   Variable은 "[end ~ start] : #x주소", Chunk는 종류(kind)를 함께 표시한다. *)
let pp_stack_shape (slots : stack_slot list) (_var_set : var_set) =
  Format.printf "=== Virtual Stack Shape ===@.";
  List.iter (fun slot ->
    let offset_str = offset_to_hex slot.start_offset in
    match slot.entry with
    | Variable ->
      Format.printf "  [%d ~ %d] : %s@."
        slot.end_offset slot.start_offset offset_str
    | Chunk kind ->
      let kind_str = match kind with
        | Unknown -> "chunk"
        | ArrayKind -> "array"
        | StructKind -> "struct"
      in
      Format.printf "  [%d ~ %d] : %s (%s)@."
        slot.end_offset slot.start_offset kind_str offset_str
  ) slots

(* [pp_json slots oc]
   스택 슬롯 목록을 "variables" / "arrays" / "structs" / "unknown"
   네 키로 분리해 JSON 형태로 out_channel에 출력한다. *)
let pp_json (slots : stack_slot list) (oc : out_channel) =
  let fmt = Format.formatter_of_out_channel oc in

  (* kind별로 chunk 슬롯 분류 *)
  let variables = List.filter (fun s -> s.entry = Variable) slots in
  let chunks_of kind =
    List.filter (fun s ->
      match s.entry with Chunk k -> k = kind | _ -> false
    ) slots
  in
  let arrays  = chunks_of ArrayKind in
  let structs = chunks_of StructKind in
  let unknown = chunks_of Unknown in

  let print_list ~last key items =
    Format.fprintf fmt "  \"%s\": [@\n" key;
    let n = List.length items in
    List.iteri (fun i slot ->
      Format.fprintf fmt "    {\"offset\": \"%s\", \"start\": %d, \"end\": %d}"
        (offset_to_hex slot.start_offset)
        slot.start_offset
        slot.end_offset;
      if i < n - 1 then Format.fprintf fmt ",";
      Format.fprintf fmt "@\n"
    ) items;
    (* 마지막 리스트가 아니면 뒤에 콤마 *)
    if last then Format.fprintf fmt "  ]@\n"
    else Format.fprintf fmt "  ],@\n"
  in

  Format.fprintf fmt "{@\n";
  print_list ~last:false "variables" variables;
  print_list ~last:false "arrays" arrays;
  print_list ~last:false "structs" structs;
  print_list ~last:true  "unknown" unknown;
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