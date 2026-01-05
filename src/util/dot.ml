let pp_id fmt s = 
  Format.fprintf fmt "\"%s\"" s

module Shape = struct
  type t = Record
  let pp fmt (s:t) =
    match s with 
    | Record -> Format.fprintf fmt "record"
end

module Color = struct
  type t = White | LightPink | Lavenderblush | LightGrey
  let pp fmt (c:t) = 
    match c with
    | White -> Format.fprintf fmt "white"
    | LightPink -> Format.fprintf fmt "lightpink"
    | Lavenderblush -> Format.fprintf fmt "lavenderblush"
    | LightGrey -> Format.fprintf fmt "\"#f1f3f5\""
end

module Style = struct
  type t = Filled
    let pp fmt (s:t) = 
    match s with
    | Filled -> Format.fprintf fmt "filled"
end

module Attr = struct
  type t = Label of string | Shape of Shape.t 
  | Color of Color.t | Style of Style.t 
  | FillColor of Color.t | ToolTip of string
  let pp fmt (a:t) = 
    match a with 
    | Label s -> Format.fprintf fmt "label=%a" pp_id s 
    | Shape s -> Format.fprintf fmt "shape=%a" Shape.pp s
    | Color c -> Format.fprintf fmt "color=%a" Color.pp c
    | Style s -> Format.fprintf fmt "style=%a" Style.pp s
    | FillColor c -> Format.fprintf fmt "fillcolor=%a" Color.pp c
    | ToolTip s -> Format.fprintf fmt "tooltip=%a" pp_id s 
end

module Edge_Stmt = struct
  type t = {id:string; edgeRHS:string; port:int}
  let pp fmt (s:t) = 
    Format.fprintf fmt "%a:s -> %a" pp_id s.id pp_id s.edgeRHS 
end

(* module Attr_Stmt = struct
  type t = Graph | Node | Edge
end *)

module Node_Stmt = struct
  type t = {id:string; attr_list:Attr.t list}
  let pp fmt (s : t) = 
    Format.fprintf fmt "%a [%a]" pp_id s.id 
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") 
    (fun fmt a -> Format.fprintf fmt "%a" Attr.pp a)) s.attr_list
end

module Stmt = struct
  type t = Node_Stmt of Node_Stmt.t 
          | Edge_Stmt of Edge_Stmt.t
          (* | Attr_Stmt of Attr_Stmt.t *)
          (* | SubGraph  *)
  
  let pp fmt (s : t) = 
    match s with
    | Node_Stmt s -> Format.fprintf fmt "%a" Node_Stmt.pp s
    | Edge_Stmt s -> Format.fprintf fmt "%a" Edge_Stmt.pp s

end

module DIGraph = struct
  type t = {id:string; stmt_list:Stmt.t list}
  
  let pp fmt (g : t) = 
    Format.fprintf fmt "digraph %a {\n%a\n}\n" pp_id g.id 
      (fun fmt -> List.iter (fun s -> Format.fprintf fmt "   %a;\n" Stmt.pp s)) g.stmt_list

end


