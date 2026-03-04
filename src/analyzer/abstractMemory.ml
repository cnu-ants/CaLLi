module F = Format


(* Interface for Abstract Memory required in LlvmAnalyzer*)
module type S =
  sig 
    (* type key *)
    type t
    type valty

    val empty : t
    val bot : t
    val is_bot : t -> bool
    (* val find_opt : string * int * int -> t -> valty option
    val find : string * int * int -> t -> valty
    val update : string * int * int -> valty -> t -> t *)
    val find_opt : string -> t -> valty option
    val find : string -> t -> valty
    val update : string -> valty -> t -> t
    val (<=) : t -> t -> bool
    val join : t -> t -> t
    val meet : t -> t -> t
    val widen : t -> t -> t
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
    
    val bindings : t -> (string * valty) list

    val filter_mem : t -> t -> t
    val group_by_value_with_env : t -> string Env.M.t -> (string * string list) list
    val pp_grouped_json_with_env : Format.formatter -> t -> string Env.M.t -> unit
    val widen_with_bb : string -> t -> t -> t

    val join_without_top : t -> t -> t
  end

(* 
  Functor building an implementation of the abstract memory 
given abstract value type.

  The abstract memory is structured as a map, where the key is an address 
and the value is an abstract value. The address is a tuple consisting of the 
basicblock name, the index of the instruction, and an random number for 
uniqueness in memory addresses.

 *)
module Make(AbsVal : AbstractDomain.S) : (S with type valty = AbsVal.t) = 
  struct
    
    (* type key = string * int * int *)
    type key = string
    module M = Map.Make 
    (struct 
      type t = key 
      let compare = compare 
    end)
    
    type t = MemBot | Mem of AbsVal.t M.t
    type valty = AbsVal.t

    let empty = Mem (M.empty)
    let bot = MemBot

    let is_bot m = 
      if m = empty then true
      else if m = bot then true
      else false

    let pp fmt mem =
      match mem with
      | MemBot -> Format.fprintf fmt "bot\n"
      | Mem mem ->
        F.fprintf fmt "[%a]\n" (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
          (fun fmt ((s), v) -> F.fprintf fmt " %s ↦ %a" s AbsVal.pp v)) (M.bindings mem)

    let find_opt x mem = 
      match mem with
      | Mem mem -> 
        M.find_opt x mem
      | MemBot -> None

    let find x mem = 
      match mem with
      | Mem mem ->
        if x = "" then AbsVal.top else
          (try M.find x mem
          with _ -> AbsVal.top)
      | MemBot -> AbsVal.bot

    let rec update x (v: AbsVal.t) mem = 
      match mem with 
      | Mem mem -> Mem (M.add x v mem)
      | MemBot -> update x v empty

    let (<=) mem1 mem2 = 
      match mem1, mem2 with
      | MemBot, _ -> true
      | Mem mem1, Mem mem2 -> 
        M.for_all (fun x v -> M.mem x mem2 && AbsVal.(v <= (M.find x mem2))) mem1
      | _, MemBot -> false

    let equal mem1 mem2 =
      mem1 <= mem2 && mem2 <= mem1

    let join mem1 mem2 =
      match mem1, mem2 with
      | MemBot, _ -> mem2
      | _, MemBot -> mem1
      | Mem mem1, Mem mem2 ->
        let mem = Mem (M.union (fun _ v1 v2 -> Some AbsVal.(join v1 v2)) mem1 mem2) in 
        if mem = empty then MemBot else mem

    let join_without_top mem1 mem2 =
      begin
      match mem1, mem2 with
      | MemBot, _ -> mem2
      | _, MemBot -> mem1
      | Mem m1, Mem m2 ->
        let joined = M.union (fun key v1 v2 ->
          if AbsVal.is_top v2 then 
            Some v1 
          else
            Some (AbsVal.join v1 v2) 
        ) m1 m2 in
        Mem joined
      end

    let widen mem1 mem2 = 
      match mem1, mem2 with
      | Mem m1, Mem m2 ->
        let widen_mem = Mem (M.union 
          (fun name v1 v2 ->
            (* let _ = Format.printf "WIDEN_MEM : %s  %a %a@." name AbsVal.pp v1 AbsVal.pp v2 in *)
            (* Some (AbsVal.widen v1 v2 ) *)
            Some (AbsVal.widen name v1 v2)
          ) m1 m2) in 
        widen_mem 
        (*let _ = Format.printf "mem 1@. %a@. mem2@. %a@. mem3@.  %a@." pp mem1 pp mem2 pp widen_mem in widen_mem *)
      | MemBot, MemBot -> MemBot
      | MemBot, Mem _ -> mem2
      | _ -> failwith "mem widen..."

    let widen_with_bb bb_name mem1 mem2 =
      match mem1, mem2 with
      | Mem m1, Mem m2 ->
        Mem (M.union
          (fun name v1 v2 ->
            let key = bb_name ^ "::" ^ name in
            Some (AbsVal.widen key v1 v2)
          ) m1 m2)
      | MemBot, MemBot -> MemBot
      | MemBot, Mem _ -> mem2
      | _ -> failwith "mem widen..."
  
    let meet _ _ =
      failwith "not implemented"

    let filter_mem mem1 mem2 = 
      match (mem1, mem2) with 
      | MemBot, _ | _, MemBot -> MemBot
      | Mem m1, Mem m2 -> 
          let filtered = M.filter (fun key val1 ->
            match M.find_opt key m2 with 
            | Some val2 -> (not (AbsVal.equal val1 val2)) && (AbsVal.is_singleton val1)
            | None -> false
          ) m1 in
          if M.is_empty filtered then MemBot else Mem filtered

    let group_by_value_with_env (mem : t) (env : string Env.M.t) 
        : (string * string list) list =
      match mem with
      | MemBot -> []
      | Mem m ->
          let tbl = Hashtbl.create 100 in
          M.iter (fun key absval ->
            match AbsVal.extract_value_string absval with
            | Some value ->
                let var_name = Env.find key env in
                if var_name <> "" then begin
                  let keys = 
                    try Hashtbl.find tbl value 
                    with Not_found -> [] 
                  in
                  Hashtbl.replace tbl value (var_name :: keys)
                end
            | None -> ()
          ) m;
          Hashtbl.fold (fun value keys acc -> (value, keys) :: acc) tbl []
    

    let transform_key (value_str : string) : string =
      try
        let value = int_of_string value_str in
        let offset = value - 100000 in
        if offset >= 0 then
          Printf.sprintf "0x%x" offset
        else
          (* 음수를 32비트 unsigned로 변환 *)
          Printf.sprintf "#x%lx" (Int32.of_int offset)
      with _ -> value_str  (* 파싱 실패하면 원본 유지 *)

    let extract_var_name (key : string) : string option =
      try
        let idx = String.rindex key '%' in
        Some (String.sub key idx (String.length key - idx))
      with Not_found -> None  (* '%' 없으면 None *)
    
    let pp_grouped_json_with_env fmt (mem : t) (env : string Env.M.t) =
      let grouped = group_by_value_with_env mem env in
      F.fprintf fmt "{@[<v>";
      List.iteri (fun i (value, keys) ->
        let transformed_value = transform_key value in
        (* '%' 있는 키만 필터링 *)
        let valid_keys = List.filter_map extract_var_name keys in
        
        if valid_keys <> [] then begin  (* 유효한 키가 있을 때만 출력 *)
          F.fprintf fmt "@,  \"%s\": [@[<v>" transformed_value;
          List.iteri (fun j short_key ->
            F.fprintf fmt "@,    {\"var\": \"%s\"}" short_key;
            if j < List.length valid_keys - 1 then F.fprintf fmt ","
          ) valid_keys;
          F.fprintf fmt "@]@,  ]";
          if i < List.length grouped - 1 then F.fprintf fmt ","
        end
      ) grouped;
      F.fprintf fmt "@]@,}"
      
    let bindings mem =
      match mem with
      | MemBot -> []
      | Mem m -> M.bindings m
          
  end
