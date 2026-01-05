  exception Unknown_id of Z.t

  let base = Z.of_int 100000
  let step = Z.of_int 10

  module ZTbl = Hashtbl.Make(struct
    type t = Z.t
    let equal = Z.equal
    let hash = Hashtbl.hash
  end)

  let next_idx = ref Z.zero

  let s2i : (string, Z.t) Hashtbl.t = Hashtbl.create 1024
  let i2s : string ZTbl.t = ZTbl.create 1024

  let make_id (idx : Z.t) : Z.t =
    Z.add base (Z.mul idx step)

  let id_of_string (s : string) : Z.t =
    match Hashtbl.find_opt s2i s with
    | Some id -> id
    | None ->
        let idx = !next_idx in
        next_idx := Z.succ !next_idx;
        let id = make_id idx in
        Hashtbl.add s2i s id;
        ZTbl.add i2s id s;
        id

  let string_of_id_opt (id : Z.t) : string option =
    ZTbl.find_opt i2s id

  let string_of_id (id : Z.t) : string =
    match string_of_id_opt id with
    | Some s -> s
    | None -> raise (Unknown_id id)
