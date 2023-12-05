type t = Black | Red | Green | Yellow | Blue | Magenta | Cyan


let printf ?(color=Black) fmt = 
  let fmt = 
    match color with
    | Black -> "\027[30m"^^fmt^^"\027[0m@."
    | Red -> "\027[31m"^^fmt^^"\027[0m@."
    | Green -> "\027[32m"^^fmt^^"\027[0m"
    | Yellow -> "\027[33m"^^fmt^^"\027[0m"
    | Blue -> "\027[34m"^^fmt^^"\027[0m"
    | Magenta -> "\027[35m"^^fmt^^"\027[0m"
    | Cyan -> "\027[36m"^^fmt^^"\027[0m"
  in
  Format.printf fmt