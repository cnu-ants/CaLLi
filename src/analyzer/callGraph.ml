module F = Format

exception No_Called

(** Call graph. key = function name *)
module M = Map.Make(String)
type elt = {calling:string list; called:(string*string) list}
type t = elt M.t


let empty : t = M.empty
let find = try M.find with Not_found -> raise No_Called
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
   (M.find func_name g).called |> 
   List.map 
   (fun ((called_func :string), (called_bb:string)) -> 
      let f = Module.find called_func m in
      let b = List.nth (Cfg.next (Bbpool.find called_bb !Bbpool.pool) f.cfg) 0 in
      Bbpool.find (b.bb_name) !Bbpool.pool)

let front (func_name:string) (g:t) (_ : Function.t Module.M.t) : string list = 
  (M.find func_name g).called |>
  List.map 
  (fun (_, (called_bb:string)) -> called_bb)

let make_call_graph (m : Module.t) : t =
  let init (m : Module.t) = 
    Module.fold
    (fun _ (f : Function.t) (g : t) -> 
      add f.function_name {calling=[]; called=[]} g
    )
    m.function_map empty
  in
  let call_graph = 
    Module.fold
    (fun _ (f : Function.t) (g : t) -> 
      let func_name = f.function_name in
      let g' = Cfg.fold 
        (fun bb_name _ g -> 
          let bb = Bbpool.find bb_name !Bbpool.pool in
          match bb.term with 
          | CallSite {callee; _} -> 
            let elt1 : elt = find func_name g in
            let g = add func_name {calling=callee::elt1.calling; called=elt1.called} g in
            let elt2 : elt = find callee g in
            let g = add callee {calling=elt2.calling; called=(func_name, bb_name)::elt2.called} g in
            g
          | _ -> g
        )
        f.cfg g 
      in
      g'
    )
    m.function_map (init m)
  in
  call_graph
