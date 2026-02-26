open Lwt.Infix

module Make (A : sig
  type runtime
  val get_icfg_json : unit -> string
  val step_once_json : runtime -> Yojson.Safe.t
  val get_state_for_bb_json : runtime -> string -> Yojson.Safe.t
end) =
struct
  let start ~(runtime : A.runtime) ~(interface : string) ~(port : int) : unit =
    Printexc.record_backtrace true;

    let paused = ref true in
    let running = ref false in

    let on_websocket (ws : Dream.websocket) : unit Lwt.t =
      let rec send_loop () =
        if !paused then Lwt_unix.sleep 0.05 >>= send_loop
        else
          let msg = A.step_once_json runtime in
          Dream.send ws (Yojson.Safe.to_string msg) >>= fun () ->
          match msg with
          | `Assoc [ ("type", `String "done") ] ->
              paused := true;
              running := false;
              Lwt.return_unit
          | _ -> Lwt_unix.sleep 0.10 >>= send_loop
      in

      let send_one_step () =
        let msg = A.step_once_json runtime in
        Dream.send ws (Yojson.Safe.to_string msg)
      in

      let rec recv_loop () =
        Dream.receive ws >>= function
        | None -> Lwt.return_unit
        | Some txt ->
            let cmd = String.trim txt in
            if String.equal cmd "play" then (
              paused := false;
              if not !running then (
                running := true;
                Lwt.async send_loop
              );
              recv_loop ()
            ) else if String.equal cmd "pause" then (
              paused := true;
              recv_loop ()
            ) else if String.equal cmd "step" then (
              paused := true;
              send_one_step () >>= fun () -> recv_loop ()
            ) else
              recv_loop ()
      in
      recv_loop ()
    in

    let state_handler req =
      match Dream.query req "bb" with
      | None -> Dream.respond ~status:`Bad_Request "missing query param: bb"
      | Some bb ->
          let j = A.get_state_for_bb_json runtime bb in
          Dream.json (Yojson.Safe.to_string j)
    in

    Dream.run ~interface ~port
    @@ Dream.logger
    @@ Dream.router
         [
           Dream.get "/icfg" (fun _req -> Dream.json (A.get_icfg_json ()));
           Dream.get "/state" state_handler;
           Dream.get "/ws" (fun _req -> Dream.websocket on_websocket);
         ]
end
