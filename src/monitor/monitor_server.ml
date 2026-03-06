open Lwt.Infix

module type WEB_ANALYZER = sig
  type runtime
  val get_icfg_json : unit -> string

  type step_ev = Done | Stepped of string
  val step_once : runtime -> step_ev

  val snapshot_json : runtime -> Yojson.Safe.t
  val get_state_for_bb_json : runtime -> string -> Yojson.Safe.t
  val get_all_states_json : runtime -> Yojson.Safe.t

  val restart : runtime -> unit
end

module Make (A : WEB_ANALYZER) =
struct
  module SS = Set.Make (String)

  type cmd =
    | Play
    | Pause
    | Step
    | Restart
    | BpSet of { bb : string; enabled : bool }
    | BpSync of { bbs : string list }

  type run_req = Continue | Budget of int

  let env_json () : Yojson.Safe.t =
    let items = ref [] in
    Env.iter
      (fun k v -> items := (`Assoc [ ("var", `String k); ("addr", `String v) ]) :: !items)
      !(Env.env);
    `Assoc [ ("items", `List (List.rev !items)) ]

  let parse_cmd (txt : string) : cmd option =
    let t = String.trim txt in
    if t = "" then None
    else if t.[0] = '{' then
      (try
         let j = Yojson.Safe.from_string t in
         let open Yojson.Safe.Util in
         let c = j |> member "cmd" |> to_string in
         match c with
         | "play" -> Some Play
         | "pause" -> Some Pause
         | "step" -> Some Step
         | "restart" -> Some Restart
         | "bp_set" ->
             let bb = j |> member "bb" |> to_string in
             let enabled = j |> member "enabled" |> to_bool in
             Some (BpSet { bb; enabled })
         | "bp_sync" ->
             let bbs = j |> member "bbs" |> to_list |> List.map to_string in
             Some (BpSync { bbs })
         | _ -> None
       with _ -> None)
    else
      match t with
      | "play" -> Some Play
      | "pause" -> Some Pause
      | "step" -> Some Step
      | "restart" -> Some Restart
      | _ -> None

  let breakpoints_json (bps : SS.t) : Yojson.Safe.t =
    `Assoc
      [
        ("type", `String "breakpoints");
        ("bbs", `List (SS.elements bps |> List.map (fun s -> `String s)));
      ]

  let add_meta (j : Yojson.Safe.t) ~(ran : int) ~(reason : string) : Yojson.Safe.t =
    match j with
    | `Assoc kv -> `Assoc (("ran", `Int ran) :: ("reason", `String reason) :: kv)
    | _ -> j

  let start ~(runtime : A.runtime) ~(interface : string) ~(port : int) : unit =
    Printexc.record_backtrace true;

    let conn_counter = ref 0 in

    let on_websocket (ws : Dream.websocket) : unit Lwt.t =
      incr conn_counter;
      let cid = !conn_counter in
      let logf fmt =
        Format.kasprintf
          (fun s -> prerr_endline (Printf.sprintf "[ws:%d] %s" cid s))
          fmt
      in

      logf "connected";

      let paused = ref true in
      let running = ref false in
      let req : run_req option ref = ref None in
      let bps = ref SS.empty in

      let closed = ref false in

      let send_mutex = Lwt_mutex.create () in
      let send_json (j : Yojson.Safe.t) : unit =
        Lwt.async (fun () ->
          Lwt_mutex.with_lock send_mutex (fun () ->
            Lwt.catch
              (fun () ->
                 let s = Yojson.Safe.to_string j in
                 logf "send len=%d" (String.length s);
                 Dream.send ws s)
              (fun exn ->
                 logf "send error=%s" (Printexc.to_string exn);
                 Lwt.return_unit)))
      in

      let stop () =
        paused := true;
        req := None
      in

      let rec stop_and_wait () : unit Lwt.t =
        stop ();
        if !running then Lwt.pause () >>= stop_and_wait else Lwt.return_unit
      in

      let max_steps_per_slice = 200 in

      let rec run_continue (fuel : int) (ran : int) : unit Lwt.t =
        if !paused then Lwt.return_unit
        else
          match A.step_once runtime with
          | A.Done ->
              stop ();
              send_json (`Assoc [ ("type", `String "done"); ("ran", `Int ran); ("reason", `String "done") ]);
              Lwt.return_unit
          | A.Stepped bb_name ->
              let ran' = ran + 1 in
              if SS.mem bb_name !bps then (
                stop ();
                send_json (add_meta (A.snapshot_json runtime) ~ran:ran' ~reason:"breakpoint");
                Lwt.return_unit
              ) else if fuel <= 0 then
                Lwt.pause () >>= fun () -> run_continue max_steps_per_slice ran'
              else
                run_continue (fuel - 1) ran'
      in

      let rec run_budget (n : int) (fuel : int) (ran : int) : unit Lwt.t =
        if !paused then Lwt.return_unit
        else if n <= 0 then (
          stop ();
          if ran > 0 then send_json (add_meta (A.snapshot_json runtime) ~ran ~reason:"step");
          Lwt.return_unit
        ) else
          match A.step_once runtime with
          | A.Done ->
              stop ();
              send_json (`Assoc [ ("type", `String "done"); ("ran", `Int ran); ("reason", `String "done") ]);
              Lwt.return_unit
          | A.Stepped _ ->
              let ran' = ran + 1 in
              if fuel <= 0 then
                Lwt.pause () >>= fun () -> run_budget n max_steps_per_slice ran'
              else
                run_budget (n - 1) (fuel - 1) ran'
      in

      let rec run_loop () : unit Lwt.t =
        Lwt.catch
          (fun () ->
             if !paused then (running := false; Lwt.return_unit)
             else
               match !req with
               | None -> (running := false; Lwt.return_unit)
               | Some Continue ->
                   run_continue max_steps_per_slice 0 >>= fun () ->
                   running := false;
                   Lwt.return_unit
               | Some (Budget n) ->
                   run_budget n max_steps_per_slice 0 >>= fun () ->
                   running := false;
                   Lwt.return_unit)
          (fun exn ->
             running := false;
             stop ();
             send_json (`Assoc [ ("type", `String "error"); ("msg", `String ("Engine Error: " ^ Printexc.to_string exn)) ]);
             Lwt.return_unit)
      in

      let request_run (r : run_req) : unit =
        paused := false;
        req := Some r;
        if not !running then (
          running := true;
          Lwt.async run_loop
        )
      in

      let handle_cmd (c : cmd) : unit Lwt.t =
        let cmd_name =
          match c with
          | Play -> "play"
          | Pause -> "pause"
          | Step -> "step"
          | Restart -> "restart"
          | BpSet _ -> "bp_set"
          | BpSync _ -> "bp_sync"
        in
        logf "handle_cmd=%s running=%b paused=%b" cmd_name !running !paused;

        Lwt.catch
          (fun () ->
             match c with
             | Play ->
                 request_run Continue;
                 Lwt.return_unit
             | Pause ->
                 stop ();
                 Lwt.return_unit
             | Step ->
                 request_run (Budget 1);
                 Lwt.return_unit
             | Restart ->
                 stop_and_wait () >>= fun () ->
                 logf "restart begin";
                 A.restart runtime;
                 logf "restart end";
                 send_json (add_meta (A.snapshot_json runtime) ~ran:0 ~reason:"restart");
                 Lwt.return_unit
             | BpSet { bb; enabled } ->
                 bps := (if enabled then SS.add bb !bps else SS.remove bb !bps);
                 send_json (breakpoints_json !bps);
                 Lwt.return_unit
             | BpSync { bbs } ->
                 bps := List.fold_left (fun acc x -> SS.add x acc) SS.empty bbs;
                 send_json (breakpoints_json !bps);
                 Lwt.return_unit)
          (fun exn ->
             logf "handle_cmd error=%s" (Printexc.to_string exn);
             Lwt.return_unit)
      in

      let cmd_queue = Queue.create () in
      let cmd_cond = Lwt_condition.create () in

      let wake () = Lwt_condition.broadcast cmd_cond () in

      let rec cmd_processor_loop () : unit Lwt.t =
        if !closed && Queue.is_empty cmd_queue then (
          logf "processor exit";
          Lwt.return_unit
        ) else if Queue.is_empty cmd_queue then
          Lwt_condition.wait cmd_cond >>= fun () -> cmd_processor_loop ()
        else
          let c = Queue.pop cmd_queue in
          logf "dequeue queue_len=%d" (Queue.length cmd_queue);
          handle_cmd c >>= fun () ->
          cmd_processor_loop ()
      in
      Lwt.async cmd_processor_loop;

      let rec recv_loop () : unit Lwt.t =
        Dream.receive ws >>= function
        | None ->
            logf "recv None (closed)";
            closed := true;
            stop ();
            Queue.clear cmd_queue;
            wake ();
            Lwt.return_unit
        | Some txt -> (
            let s =
              if String.length txt > 200 then String.sub txt 0 200 ^ "..."
              else txt
            in
            logf "recv raw=%S" s;
            match parse_cmd txt with
            | None ->
                logf "parse_cmd: none";
                recv_loop ()
            | Some Restart ->
                logf "parse_cmd: restart (clear queue)";
                stop ();
                Queue.clear cmd_queue;
                Queue.push Restart cmd_queue;
                logf "queue_len=%d" (Queue.length cmd_queue);
                wake ();
                recv_loop ()
            | Some c ->
                logf "parse_cmd: enqueue";
                Queue.push c cmd_queue;
                logf "queue_len=%d" (Queue.length cmd_queue);
                wake ();
                recv_loop ()
          )
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

    let states_handler _req =
      Dream.json (Yojson.Safe.to_string (A.get_all_states_json runtime))
    in

    Dream.run ~interface ~port
    @@ Dream.logger
    @@ Dream.router
         [
           Dream.get "/icfg" (fun _req -> Dream.json (A.get_icfg_json ()));
           Dream.get "/state" state_handler;
           Dream.get "/states" states_handler;
           Dream.get "/env" (fun _req -> Dream.json (Yojson.Safe.to_string (env_json ())));
           Dream.get "/ws" (fun _req -> Dream.websocket on_websocket);
         ]
end
