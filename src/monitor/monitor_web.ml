module type WEB_ANALYZER = Monitor_server.WEB_ANALYZER

module Make (A : WEB_ANALYZER) = struct
  module Server = Monitor_server.Make (A)

  type frontend =
    | Disabled
    | Static
    | Dev

  let start
      ?(frontend = Static)
      ?(frontend_cmd = [ "npm"; "run"; "dev" ])
      ~(calli_home : string)
      ~(runtime : A.runtime)
      ~(interface : string)
      ~(port : int)
      ()
      : unit =
    let extra_routes =
      match frontend with
      | Disabled -> []
      | Static -> Frontend_static.routes ~calli_home ()
      | Dev ->
          ignore (Frontend_launcher.spawn ~calli_home ~argv:frontend_cmd ());
          []
    in
    Server.start ~extra_routes ~runtime ~interface ~port
end
