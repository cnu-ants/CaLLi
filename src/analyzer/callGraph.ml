module F = Format

exception No_Called

(** Call graph. key = function name *)
module M = Map.Make(String)
type elt = {calling:string list; called:(string*string) list}
type t = elt M.t


let empty : t = M.empty
let find k g = try M.find k g with | _ -> raise No_Called
let find_opt = M.find_opt
let add = M.add
let fold = M.fold

let pp fmt (g : t) = 
  let print_calling_list _ v = 
    List.iter
    (fun s -> F.printf "%s " s)
    v
  in
  let print_called_list _ v = 
    List.iter
    (fun (_, s2) -> F.printf "%s" s2)
    v
  in
  F.fprintf fmt "%a" (F.pp_print_list 
    ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
    (fun fmt (s, e) -> 
      F.fprintf fmt "%s -> \n calling : %a\n called :%a" s print_calling_list e.calling print_called_list e.called
    ))
    (M.bindings g)

let next (func_name:string) (g:t) (m : Function.t Module.M.t) : Basicblock.t list = 
   let _ = Format.printf "callGraph next@." in
   (find func_name g).called |> 
   List.map 
   (fun ((called_func :string), (called_bb:string)) -> 
      let f = Module.find called_func m in
      let b = List.nth (Cfg.next (Bbpool.find called_bb !Bbpool.pool) f.cfg) 0 in
      Bbpool.find_bb (b.bb_name))

let front (func_name:string) (g:t) (_ : Function.t Module.M.t) : string list = 
   let _ = Format.printf "callGraph front@." in
  (find func_name g).called |>
  List.map 
  (fun (_, (called_bb:string)) -> called_bb)

let make_call_graph (m : Module.t) : t =
   let _ = Format.printf "callGraph make@." in
  let init (m : Module.t) = 
    Module.fold
    (fun _ (f : Function.t) (g : t) -> 
      add f.function_name {calling=[]; called=[]} g
    )
    m.function_map empty
  in
  let _ = Format.printf "callGraph make init done@." in
  let call_graph = 
    Module.fold
    (fun _ (f : Function.t) (g : t) -> 
      let func_name = f.function_name in
      let g' = Cfg.fold 
        (fun bb_name _ g -> 
          let bb = Bbpool.find_bb bb_name in

          match bb.term with 
          | Some CallSite {callee; _} -> 
            let elt1 : elt = find func_name g in
            let g = add func_name {calling=callee::elt1.calling; called=elt1.called} g in
            let elt2 = find_opt callee g in
            let g = 
              (match elt2 with
              | None -> g
              | Some elt2 -> add callee {calling=elt2.calling; called=(func_name, bb_name)::elt2.called} g
              ) in
            g
          | _ -> g
        )
        f.cfg g 
      in
      g'
    )
    m.function_map (init m)
  in
  let _ = Format.printf "callGraph make done@." in
  call_graph
