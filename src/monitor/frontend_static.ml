open Lwt.Infix
open Printf

let dist_dir ~(calli_home : string) : string =
  Filename.concat (Filename.concat calli_home "monitor") "dist"

let index_html ~(calli_home : string) : string =
  Filename.concat (dist_dir ~calli_home) "index.html"

let is_reg (path : string) : bool =
  try (Unix.stat path).Unix.st_kind = Unix.S_REG with _ -> false

let ends_with ~suffix s =
  let ls = String.length s in
  let lf = String.length suffix in
  ls >= lf && String.sub s (ls - lf) lf = suffix

let has_ext_like_asset (rel : string) : bool =
  let base = Filename.basename rel in
  try
    ignore (String.index base '.');
    true
  with Not_found -> false

let content_type (path : string) : string =
  if ends_with ~suffix:".html" path then "text/html; charset=utf-8"
  else if ends_with ~suffix:".js" path || ends_with ~suffix:".mjs" path then "application/javascript; charset=utf-8"
  else if ends_with ~suffix:".css" path then "text/css; charset=utf-8"
  else if ends_with ~suffix:".json" path then "application/json; charset=utf-8"
  else if ends_with ~suffix:".map" path then "application/json; charset=utf-8"
  else if ends_with ~suffix:".svg" path then "image/svg+xml"
  else if ends_with ~suffix:".png" path then "image/png"
  else if ends_with ~suffix:".jpg" path || ends_with ~suffix:".jpeg" path then "image/jpeg"
  else if ends_with ~suffix:".gif" path then "image/gif"
  else if ends_with ~suffix:".webp" path then "image/webp"
  else if ends_with ~suffix:".ico" path then "image/x-icon"
  else if ends_with ~suffix:".woff2" path then "font/woff2"
  else if ends_with ~suffix:".woff" path then "font/woff"
  else if ends_with ~suffix:".ttf" path then "font/ttf"
  else "application/octet-stream"

let read_file path =
  Lwt_io.(with_file ~mode:Input path read)

let respond_file path =
  read_file path >>= fun body ->
  Dream.respond
    ~headers:[ ("Content-Type", content_type path) ]
    body

let not_found () =
  Dream.respond ~status:`Not_Found "Not found"

let is_safe_segment (s : string) : bool =
  not (String.equal s "." || String.equal s ".." || String.contains s '\000')

let drop_query_and_fragment (s : string) : string =
  let cut_at ch str =
    try
      let i = String.index str ch in
      String.sub str 0 i
    with Not_found -> str
  in
  s |> cut_at '?' |> cut_at '#'

let trim_leading_slash (s : string) : string =
  if String.length s > 0 && s.[0] = '/' then
    String.sub s 1 (String.length s - 1)
  else
    s

let route_to_relpath (req : Dream.request) : string =
  Dream.target req
  |> drop_query_and_fragment
  |> trim_leading_slash

let serve ~(calli_home : string) (req : Dream.request) =
  let dist_dir = dist_dir ~calli_home in
  let index_html = index_html ~calli_home in
  if not (is_reg index_html) then
    Dream.respond
      ~status:`Internal_Server_Error
      (sprintf
         "built frontend not found: %s\nrun: (cd %s/monitor && npm run build)"
         index_html
         calli_home)
  else
    let rel = route_to_relpath req in
    let segments =
      if rel = "" then [] else String.split_on_char '/' rel
    in
    if not (List.for_all is_safe_segment segments) then
      not_found ()
    else
      let requested =
        if rel = "" then index_html
        else Filename.concat dist_dir rel
      in
      if is_reg requested then
        respond_file requested
      else if rel <> "" && has_ext_like_asset rel then
        not_found ()
      else
        respond_file index_html

let routes ~(calli_home : string) () : Dream.route list =
  [
    Dream.get "/" (serve ~calli_home);
    Dream.get "/**" (serve ~calli_home);
  ]
