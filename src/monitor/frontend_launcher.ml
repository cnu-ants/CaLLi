open Printf

type process =
  {
    pid : int;
  }

let monitor_dir ~(calli_home : string) : string =
  Filename.concat calli_home "monitor"

let package_json ~(calli_home : string) : string =
  Filename.concat (monitor_dir ~calli_home) "package.json"

let with_chdir (dir : string) (f : unit -> 'a) : 'a =
  let cwd = Sys.getcwd () in
  Unix.chdir dir;
  try
    let x = f () in
    Unix.chdir cwd;
    x
  with exn ->
    Unix.chdir cwd;
    raise exn

let spawn
    ~(calli_home : string)
    ?(argv = [ "npm"; "run"; "dev" ])
    ()
    : process =
  let mon_dir = monitor_dir ~calli_home in
  let pkg = package_json ~calli_home in

  if not (Sys.file_exists pkg) then
    failwith (sprintf "frontend package.json not found: %s" pkg);

  eprintf "[frontend-dev] starting in %s: %s\n%!"
    mon_dir
    (String.concat " " argv);

  let pid =
    with_chdir mon_dir (fun () ->
        Unix.create_process
          "/usr/bin/env"
          (Array.of_list ("env" :: argv))
          Unix.stdin
          Unix.stdout
          Unix.stderr)
  in

  at_exit (fun () ->
      try Unix.kill pid Sys.sigterm with _ -> ());

  { pid }

let stop (p : process) : unit =
  try Unix.kill p.pid Sys.sigterm with _ -> ()
