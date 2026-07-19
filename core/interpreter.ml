open Types

(*
self = {
  name = string
  atoms = []         -- all space-separated words of this particle
  arg = string       -- all particles but the first joined by space
  matched_groups = []|nil   -- matched groups of the pattern (if present)
  [LATER] attribute = string -- all attributes returned by child nodes (except aftertext) in HTML format
  subparticles = [subparticle1.self, subparticle2.self, ...]
}
*)
let rec particle_lua_self (p : particle) =
  let escape_lua_str (s : string) : string =
    s |> String.to_seq |> List.of_seq
    |> List.map (function
      | '"' -> "\\\""
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | c -> String.make 1 c)
    |> String.concat "" |> Printf.sprintf {|"%s"|}
  in

  let atoms_lua =
    p.atoms |> List.map escape_lua_str |> String.concat ", "
    |> Printf.sprintf "{%s}"
  in

  let matched_groups_lua =
    match p.matched_groups with
    | None -> "nil"
    | Some groups ->
        groups |> List.map escape_lua_str |> String.concat ", "
        |> Printf.sprintf "{%s}"
  in

  let subparticles_lua =
    p.subparticles |> List.map particle_lua_self |> String.concat ", "
    |> Printf.sprintf "{%s}"
  in

  let arg = p.atoms |> List.tl |> String.concat " " |> escape_lua_str in

  Printf.sprintf
    {|{
    name = %s,
    atoms = %s,
    arg = %s,
    matched_groups = %s,
    subparticles = %s
}|}
    (escape_lua_str p.parser.name)
    atoms_lua arg matched_groups_lua subparticles_lua

let replace_list_access (name : string) (list : string list) (s : string) :
    string =
  let re = Printf.sprintf "\\$%s\\[\\([0-9]+\\)\\]" name |> Str.regexp in
  try
    Str.global_substitute re
      (fun matched ->
        let i = Str.matched_group 1 matched |> int_of_string in
        List.nth list i)
      s
  with _ -> s

let evaluate_parser_value ?(replaced_text : string option = None) (p : particle)
    ~(content : string) ~(parent_html : string) (pval : parser_value) : string =
  match pval with
  | ReplaceString expr -> (
      try
        expr
        |> String.replace_all ~sub:"$name" ~by:p.parser.name
        |> String.replace_all ~sub:"$content" ~by:content
        |> String.replace_all ~sub:"$arg"
             ~by:(p.atoms |> List.tl |> String.concat " ")
        |> replace_list_access "atoms" p.atoms
        |> (p.matched_groups
           |> Option.fold ~none:Fun.id ~some:(fun grp ->
               replace_list_access "matched_groups" grp))
        |> (replaced_text
           |> Option.fold ~none:Fun.id ~some:(fun r ->
               String.replace_all ~sub:"$replaced" ~by:r ~start:0))
      with _ -> "REPLACESTRING ERROR" (* TODO: More *))
  | LuaFunction lua_func ->
      let escape_lua_str (s : string) : string =
        s |> String.to_seq |> List.of_seq
        |> List.map (function
          | '"' -> "\\\""
          | '\\' -> "\\\\"
          | '\n' -> "\\n"
          | '\t' -> "\\t"
          | c -> String.make 1 c)
        |> String.concat "" |> Printf.sprintf {|"%s"|}
      in
      let globals =
        Printf.sprintf
          {|
            this = %s
            ctx = %s
            content = %s
            parent_html = %s
            replaced = %s
          |}
          (particle_lua_self p) "nil" (escape_lua_str content)
          (escape_lua_str parent_html)
          (Option.fold ~none:"nil" ~some:escape_lua_str replaced_text)
      in
      Lua_eval.eval_lua lua_func globals

let replace_first ~substring ~new_text s =
  let quoted = Str.quote substring in
  let whole_word_re = Str.regexp ("\\b" ^ quoted ^ "\\b") in
  let any_re = Str.regexp quoted in
  try
    ignore (Str.search_forward whole_word_re s 0);
    Str.replace_first whole_word_re new_text s
  with Not_found -> (
    try Str.replace_first any_re new_text s with Not_found -> s)

let group_consecutive_by_name (particles : particle list) =
  let rec aux groups current_group = function
    | [] -> List.rev (List.rev current_group :: groups)
    | p :: ps -> (
        match current_group with
        | last :: _ when last.parser.name = p.parser.name ->
            aux groups (p :: current_group) ps
        | _ -> aux (List.rev current_group :: groups) [ p ] ps)
  in
  match particles with [] -> [] | p :: ps -> aux [] [ p ] ps

let evaluate_wrap (wrap_replacestring : string) (html : string) =
  wrap_replacestring |> String.replace_all ~sub:"$elements" ~by:html

let rec evaluate_particles (parent_html : string) (particles : particle list) :
    string =
  let evaluate_particle = function
    | { subparticles; parser = { build_html = None; _ }; _ } -> ""
    | {
        subparticles = [];
        parser = { aftertext = None; build_html = Some build_html; _ };
        _;
      } as part ->
        evaluate_parser_value part ~content:"" ~parent_html build_html
    | {
        subparticles = [];
        parser = { aftertext = Some aft; build_html = Some build_html; _ };
        _;
      } as part ->
        let to_replace =
          evaluate_parser_value part ~content:"" ~parent_html aft
        in
        let to_replace =
          if String.is_empty to_replace then parent_html else to_replace
        in
        let replace_with =
          evaluate_parser_value part ~content:"" ~parent_html build_html
            ~replaced_text:(Some to_replace)
        in
        replace_first ~substring:to_replace ~new_text:replace_with parent_html
    | {
        subparticles;
        parser = { aftertext; build_html = Some current_build_html; _ };
        _;
      } as part ->
        (* regular children *)
        let content =
          subparticles
          |> List.filter (fun s -> Option.is_none s.parser.aftertext)
          |> evaluate_particles ""
        in
        (* the node itself *)
        let html =
          evaluate_parser_value part ~content ~parent_html current_build_html
        in
        (* aftertext children *)
        subparticles
        |> List.filter (fun s -> Option.is_some s.parser.aftertext)
        |> List.fold_left (fun acc p -> evaluate_particles acc [ p ]) html
  in
  particles |> group_consecutive_by_name
  |> List.map (fun group ->
      match group with
      | [ p ] -> evaluate_particle p
      | { parser = { list_wrap = Some w; _ }; _ } :: _ ->
          evaluate_wrap w (List.map evaluate_particle group |> String.concat "")
      | _ :: _ -> List.map evaluate_particle group |> String.concat ""
      | [] -> "")
  |> String.concat ""
(* List.map evaluate_particle particles |> String.concat " " *)
