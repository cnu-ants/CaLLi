module F = Format

exception No_Called

module M = Map.Make (String)
module SS = Set.Make (String)

type elt = { calling : string list; called : (string * string) list }
type t = elt M.t

let empty : t = M.empty
let find k g = try M.find k g with _ -> raise No_Called
let find_opt = M.find_opt
let add = M.add
let fold = M.fold

let pp fmt (g : t) =
  let print_calling_list fmt v =
    List.iter (fun s -> F.fprintf fmt "%s " s) v
  in
  let print_called_list fmt v =
    List.iter (fun (_, s2) -> F.fprintf fmt "%s " s2) v
  in
  F.fprintf fmt "%a"
    (F.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
       (fun fmt (s, e) ->
         F.fprintf fmt "%s -> \n calling : %a\n called : %a"
           s
           print_calling_list e.calling
           print_called_list e.called))
    (M.bindings g)

let next (func_name : string) (g : t) (m : Function.t Module.M.t) : Basicblock.t list =
  let _ = Format.printf "callGraph next@." in
  (find func_name g).called
  |> List.map (fun (called_func, called_bb) ->
         let f = Module.find called_func m in
         let b = List.nth (Cfg.next (Bbpool.find called_bb !Bbpool.pool) f.cfg) 0 in
         Bbpool.find_bb b.bb_name)

let front (func_name : string) (g : t) (_ : Function.t Module.M.t) : string list =
  let _ = Format.printf "callGraph front@." in
  (find func_name g).called |> List.map (fun (_, called_bb) -> called_bb)

let make_call_graph (m : Module.t) : t =
  let _ = Format.printf "callGraph make@." in
  let init (m : Module.t) =
    Module.fold
      (fun _ (f : Function.t) (g : t) ->
        add f.function_name { calling = []; called = [] } g)
      m.function_map empty
  in
  let _ = Format.printf "callGraph make init done@." in
  let call_graph =
    Module.fold
      (fun _ (f : Function.t) (g : t) ->
        let func_name = f.function_name in
        let g' =
          Cfg.fold
            (fun bb_name _ g_acc ->
              let bb = Bbpool.find_bb bb_name in
              match bb.term with
              | Some (Term.CallSite { callee; _ }) ->
                  let elt1 : elt = find func_name g_acc in
                  let g_acc =
                    add func_name
                      { calling = callee :: elt1.calling; called = elt1.called }
                      g_acc
                  in
                  let elt2 = find_opt callee g_acc in
                  let g_acc =
                    match elt2 with
                    | None -> g_acc
                    | Some elt2 ->
                        add callee
                          {
                            calling = elt2.calling;
                            called = (func_name, bb_name) :: elt2.called;
                          }
                          g_acc
                  in
                  g_acc
              | _ -> g_acc)
            f.cfg g
        in
        g')
      m.function_map (init m)
  in
  let _ = Format.printf "callGraph make done@." in
  call_graph

let build_adj_from_map (m : Function.t Module.M.t) : string list M.t =
  let base =
    Module.fold
      (fun _ (f : Function.t) acc -> M.add f.function_name [] acc)
      m M.empty
  in
  Module.fold
    (fun _ (f : Function.t) acc ->
      let succs =
        Cfg.fold
          (fun bb_name _ acc2 ->
            let bb = Bbpool.find_bb bb_name in
            match bb.term with
            | Some (Term.CallSite { callee; _ }) -> (
                match Module.find_opt callee m with
                | Some callee_f ->
                    if List.mem callee_f.function_name acc2 then acc2
                    else callee_f.function_name :: acc2
                | None -> acc2)
            | _ -> acc2)
          f.cfg []
        |> List.sort_uniq String.compare
      in
      M.add f.function_name succs acc)
    m base

let reverse_adj (adj : string list M.t) : string list M.t =
  let base = M.fold (fun k _ acc -> M.add k [] acc) adj M.empty in
  M.fold
    (fun src dsts acc ->
      List.fold_left
        (fun acc2 dst ->
          let prev = match M.find_opt dst acc2 with Some xs -> xs | None -> [] in
          if List.mem src prev then acc2 else M.add dst (src :: prev) acc2)
        acc dsts)
    adj base

let to_neighbors_meta_json (m : Function.t Module.M.t) (focus : string) : Yojson.Safe.t =
  let adj = build_adj_from_map m in
  let rev = reverse_adj adj in

  let succs = match M.find_opt focus adj with Some xs -> xs | None -> [] in
  let preds = match M.find_opt focus rev with Some xs -> xs | None -> [] in

  let succ_count = List.length succs in
  let pred_count = List.length preds in
  let node_estimate = 1 + succ_count + pred_count in

  let too_big =
    node_estimate > 120 || succ_count > 80 || pred_count > 80
  in

  let reason =
    if too_big then
      Printf.sprintf
        "large neighborhood: preds=%d succs=%d nodes~=%d"
        pred_count succ_count node_estimate
    else
      ""
  in

  `Assoc
    [
      ("focus", `String focus);
      ("too_big", `Bool too_big);
      ("pred_count", `Int pred_count);
      ("succ_count", `Int succ_count);
      ("node_estimate", `Int node_estimate);
      ("reason", `String reason);
    ]

let graph_json_of_adj (adj : string list M.t) : Yojson.Safe.t =
  let nodes_json =
    M.bindings adj
    |> List.map (fun (fn, _) ->
           `Assoc
             [
               ("id", `String fn);
               ("label", `String fn);
             ])
  in
  let edges_json, _ =
    M.fold
      (fun src dsts (acc, idx) ->
        List.fold_left
          (fun (acc2, idx2) dst ->
            ( `Assoc
                [
                  ("id", `String ("cg" ^ string_of_int idx2));
                  ("source", `String src);
                  ("target", `String dst);
                  ("kind", `String "call");
                ]
              :: acc2,
              idx2 + 1 ))
          (acc, idx) dsts)
      adj ([], 0)
  in
  `Assoc [ ("nodes", `List nodes_json); ("edges", `List (List.rev edges_json)) ]

let to_full_json (m : Function.t Module.M.t) : Yojson.Safe.t =
  build_adj_from_map m |> graph_json_of_adj

let to_neighbors_json (m : Function.t Module.M.t) (focus : string) : Yojson.Safe.t =
  let adj = build_adj_from_map m in
  let rev = reverse_adj adj in

  let succs = match M.find_opt focus adj with Some xs -> xs | None -> [] in
  let preds = match M.find_opt focus rev with Some xs -> xs | None -> [] in

  let keep =
    List.fold_left (fun s x -> SS.add x s) (SS.singleton focus) (succs @ preds)
  in

  let sub_adj =
    M.fold
      (fun src dsts acc ->
        if SS.mem src keep then
          let dsts' = List.filter (fun dst -> SS.mem dst keep) dsts in
          M.add src dsts' acc
        else
          acc)
      adj M.empty
  in

  let sub_adj =
    SS.fold
      (fun fn acc -> if M.mem fn acc then acc else M.add fn [] acc)
      keep sub_adj
  in
  graph_json_of_adj sub_adj

let sccs_of_adj (adj : string list M.t) : string list list =
  let index = ref 0 in
  let idx_tbl : (string, int) Hashtbl.t = Hashtbl.create 251 in
  let low_tbl : (string, int) Hashtbl.t = Hashtbl.create 251 in
  let onstack_tbl : (string, bool) Hashtbl.t = Hashtbl.create 251 in
  let stack : string Stack.t = Stack.create () in
  let comps = ref [] in

  let get_tbl tbl k =
    match Hashtbl.find_opt tbl k with
    | Some v -> v
    | None -> failwith ("missing key: " ^ k)
  in

  let rec strongconnect (v : string) =
    Hashtbl.replace idx_tbl v !index;
    Hashtbl.replace low_tbl v !index;
    incr index;

    Stack.push v stack;
    Hashtbl.replace onstack_tbl v true;

    let succs_v = match M.find_opt v adj with Some xs -> xs | None -> [] in
    List.iter
      (fun w ->
        if not (Hashtbl.mem idx_tbl w) then (
          strongconnect w;
          let lv = get_tbl low_tbl v in
          let lw = get_tbl low_tbl w in
          Hashtbl.replace low_tbl v (min lv lw)
        ) else if Option.value (Hashtbl.find_opt onstack_tbl w) ~default:false then (
          let lv = get_tbl low_tbl v in
          let iw = get_tbl idx_tbl w in
          Hashtbl.replace low_tbl v (min lv iw)
        ))
      succs_v;

    if get_tbl low_tbl v = get_tbl idx_tbl v then (
      let comp = ref [] in
      let done_ = ref false in
      while not !done_ do
        let w = Stack.pop stack in
        Hashtbl.replace onstack_tbl w false;
        comp := w :: !comp;
        if String.equal w v then done_ := true
      done;
      comps := !comp :: !comps
    )
  in

  M.iter
    (fun v _ ->
      if not (Hashtbl.mem idx_tbl v) then strongconnect v)
    adj;

  !comps

module ES = Set.Make (struct
  type t = string * string
  let compare = compare
end)

let to_scc_json (m : Function.t Module.M.t) : Yojson.Safe.t =
  let adj = build_adj_from_map m in
  let sccs = sccs_of_adj adj in

  let comp_of_fn : (string, int) Hashtbl.t = Hashtbl.create 251 in
  List.iteri
    (fun i members ->
      List.iter (fun fn -> Hashtbl.replace comp_of_fn fn i) members)
    sccs;

  let nodes_json =
    List.mapi
      (fun i members ->
        let members = List.sort String.compare members in
        let label =
          match members with
          | [ x ] -> x
          | _ -> Printf.sprintf "SCC[%d]" (List.length members)
        in
        `Assoc
          [
            ("id", `String ("scc" ^ string_of_int i));
            ("label", `String label);
            ("members", `List (List.map (fun x -> `String x) members));
          ])
      sccs
  in

  let edge_set =
    M.fold
      (fun src dsts acc ->
        let csrc = "scc" ^ string_of_int (Hashtbl.find comp_of_fn src) in
        List.fold_left
          (fun acc2 dst ->
            let cdst = "scc" ^ string_of_int (Hashtbl.find comp_of_fn dst) in
            if String.equal csrc cdst then acc2 else ES.add (csrc, cdst) acc2)
          acc dsts)
      adj ES.empty
  in

  let edges_json =
    ES.elements edge_set
    |> List.mapi (fun i (src, dst) ->
           `Assoc
             [
               ("id", `String ("cg_scc_" ^ string_of_int i));
               ("source", `String src);
               ("target", `String dst);
               ("kind", `String "call");
             ])
  in

  `Assoc [ ("nodes", `List nodes_json); ("edges", `List edges_json) ]
