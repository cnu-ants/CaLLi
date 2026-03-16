let current_version = 1

let save path md bbpool =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      Marshal.to_channel oc (current_version, md, bbpool) [])

let load path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let (version, md, bbpool) = Marshal.from_channel ic in
      if version <> current_version then
        failwith
          (Format.sprintf
             "cache version mismatch: got %d, expected %d"
             version current_version);
      (md, bbpool))

let exists path =
  Sys.file_exists path
