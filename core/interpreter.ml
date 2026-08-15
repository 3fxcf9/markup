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
    content = %s,
    arg = %s,
    matched_groups = %s,
    subparticles = %s
}|}
    (escape_lua_str p.parser.name)
    atoms_lua (escape_lua_str p.content) arg matched_groups_lua subparticles_lua

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

let replace_assoc_list_access (name : string)
    (assoc_list : (string * string) list) (s : string) : string =
  let re = Printf.sprintf "\\$%s\\[\\([a-z0-9_]+\\)\\]" name |> Str.regexp in
  try
    Str.global_substitute re
      (fun matched ->
        let key = Str.matched_group 1 matched in
        List.assoc key assoc_list)
      s
  with _ -> s

let replace_external_metadata_access (reg : registry) (s : string) : string =
  let re =
    "\\$external_metadata\\[\\([a-zA-Z0-9-_/]+\\)\\]\\[\\([a-z0-9_]+\\)\\]"
    |> Str.regexp
  in
  try
    Str.global_substitute re
      (fun matched ->
        let file =
          Str.matched_group 1 matched
          |> Filename.concat !(reg.relative_path)
          |> Fs.normalize_path
        in
        let key = Str.matched_group 2 matched in
        reg.external_metadata |> List.assoc file |> List.assoc key)
      s
  with _ -> s

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

let rec evaluate_parser_value ?(replaced_text : string option = None)
    (reg : registry) (p : particle) ~(parent_html : string)
    (pval : parser_value) : string =
  match pval with
  | ReplaceString expr -> (
      try
        expr
        |> String.replace_all ~sub:"$content" ~by:p.content
        |> String.replace_all ~sub:"$arg"
             ~by:(p.atoms |> List.tl |> String.concat " " |> html_escape)
        |> replace_list_access "atoms" p.atoms
        |> replace_assoc_list_access "metadata" !(reg.metadata)
        |> replace_external_metadata_access reg
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

      let lua_of_assoc assoc =
        let grouped =
          List.fold_left
            (fun acc (key, value) ->
              let values =
                match List.assoc_opt key acc with
                | Some vs -> value :: vs
                | None -> [ value ]
              in
              (key, values) :: List.remove_assoc key acc)
            [] assoc
        in
        grouped
        |> List.map (fun (key, values) ->
            let value =
              values |> List.map escape_lua_str |> String.concat ", "
              |> Printf.sprintf "{%s}"
            in
            Printf.sprintf {|["%s"] = %s|} key value)
        |> String.concat "," |> Printf.sprintf "{%s}"
      in

      let metadata = lua_of_assoc !(reg.metadata) in
      let external_metadata =
        reg.external_metadata
        |> List.map (fun (file, meta) ->
            Printf.sprintf {|["%s"] = %s|} file (lua_of_assoc meta))
        |> String.concat "," |> Printf.sprintf "{%s}"
      in
      let globals =
        Printf.sprintf
          {|
            this = %s
            ctx = %s
            parent_html = %s
            replaced = %s
            metadata = %s
            external_metadata = %s
            relative_path = %s
            filename = %s
          |}
          (particle_lua_self p) "nil"
          (escape_lua_str parent_html)
          (Option.fold ~none:"nil" ~some:escape_lua_str replaced_text)
          metadata external_metadata
          (escape_lua_str !(reg.relative_path))
          (escape_lua_str !(reg.filename))
      in

      let markup_parser ls =
        let open Lua_api in
        let lines = LuaL.checkstring ls 1 |> String.split_on_char '\n' in
        Parser.parse_document reg lines
        |> evaluate_particles reg "" |> fst |> Lua.pushstring ls;
        1
      in
      Lua_eval.eval_lua ~markup_parser reg lua_func globals

and evaluate_wrap (wrap_replacestring : string) (html : string) =
  wrap_replacestring |> String.replace_all ~sub:"$elements" ~by:html

and evaluate_metadata (reg : registry) (part : particle) ~(parent_html : string)
    (pval_opt : parser_value option) : unit =
  pval_opt
  |> Option.iter (fun pval ->
      reg.metadata :=
        (part.parser.name, evaluate_parser_value reg part ~parent_html pval)
        :: !(reg.metadata))

and evaluate_particles (reg : registry) (parent_html : string)
    (particles : particle list) : string * particle list =
  let evaluate_particle p : string * particle =
    let output, evaluated_particle =
      match p with
      (* No produced html *)
      | { subparticles; parser = { build_html = None; metadata; _ }; _ } as part
        ->
          let content, evaluated_subparticles =
            subparticles
            |> List.filter (fun s -> Option.is_none s.parser.aftertext)
            |> evaluate_particles reg ""
          in
          evaluate_metadata reg
            { part with subparticles = evaluated_subparticles }
            ~parent_html metadata;
          ("", { part with content; subparticles = evaluated_subparticles })
      (* Non-aftertext leaf *)
      | {
          subparticles = [];
          parser =
            { aftertext = None; build_html = Some build_html; metadata; _ };
          _;
        } as part ->
          evaluate_metadata reg part ~parent_html metadata;
          (evaluate_parser_value reg part ~parent_html build_html, part)
      (* Aftertext leaf *)
      | {
          subparticles = [];
          parser =
            { aftertext = Some aft; build_html = Some build_html; metadata; _ };
          _;
        } as part ->
          evaluate_metadata reg part ~parent_html metadata;
          let to_replace = evaluate_parser_value reg part ~parent_html aft in
          let to_replace =
            if String.is_empty to_replace then parent_html else to_replace
          in
          let replace_with =
            evaluate_parser_value reg part ~parent_html build_html
              ~replaced_text:(Some to_replace)
          in
          ( replace_first ~substring:to_replace ~new_text:replace_with
              parent_html,
            part )
      (* Node *)
      | {
          subparticles;
          parser =
            { aftertext; build_html = Some current_build_html; metadata; _ };
          _;
        } as part ->
          (* regular children *)
          let content, evaluated_subparticles =
            subparticles
            |> List.filter (fun s -> Option.is_none s.parser.aftertext)
            |> evaluate_particles reg ""
          in
          let particle_with_content =
            { part with content; subparticles = evaluated_subparticles }
          in
          evaluate_metadata reg particle_with_content ~parent_html metadata;
          (* the node itself *)
          let html =
            evaluate_parser_value reg particle_with_content ~parent_html
              current_build_html
          in
          (* aftertext children *)
          let html' =
            subparticles
            |> List.filter (fun s -> Option.is_some s.parser.aftertext)
            |> List.fold_left
                 (fun acc p -> evaluate_particles reg acc [ p ] |> fst)
                 html
          in
          (html', particle_with_content)
    in
    if p.parser.head then (
      reg.head := !(reg.head) ^ output;
      ("", evaluated_particle))
    else (output, evaluated_particle)
  in
  let html, evaluated_particles =
    particles |> group_consecutive_by_name
    |> List.map (fun group ->
        match group with
        | { parser = { list_wrap = Some w; _ }; _ } :: _ ->
            let html, particles = List.split_map evaluate_particle group in
            let html = String.concat "" html in
            (evaluate_wrap w html, particles)
        | _ :: _ ->
            let html, particles = List.split_map evaluate_particle group in
            let html = String.concat "" html in
            (html, particles)
        | [] -> ("", []))
    |> List.split
  in
  (String.concat "" html, List.concat evaluated_particles)
(* |> List.fold_left *)
(*      (fun (html, tree) (grp_html, grp_particles) -> *)
(*        (html ^ grp_html, tree @ grp_particles)) *)
(*      ("", []) *)

(* List.map evaluate_particle particles |> String.concat " " *)
