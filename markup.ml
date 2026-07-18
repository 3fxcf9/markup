type particle = {
  parser : Parser.parser_def;
  atoms : string list;
  matched_groups : string list option;
  subparticles : particle list;
}

let make_raw_particle (lines : string list) : particle =
  let raw = lines |> String.concat "\n" |> String.split_on_char ' ' in
  let cue = match Parser.raw_parser.matching with `Cue c -> c | _ -> "raw" in
  {
    parser = Parser.raw_parser;
    atoms = cue :: raw;
    matched_groups = None;
    subparticles = [];
  }

let bold_cyan = Printf.sprintf "\027[36;1m%s\027[0m"

let rec pp_particle ?(indent = 0) p =
  let indent_str i = String.make (i * 2) ' ' in
  let rec pp_list pp indent lst =
    if lst = [] then "[]"
    else
      let items = lst |> List.map (fun x -> indent_str (indent + 1) ^ pp (indent + 1) x) in
      "[\n" ^ indent_str indent ^ String.concat ";\n" items ^ "\n" ^ indent_str indent ^ "]"
  in
  let print_opt_list pp indent = function
    | None -> indent_str indent ^ "None"
    | Some l -> pp_list pp indent l
  in
  let string_pp _ s = Printf.sprintf "%S" s in
  let particle_pp i p = pp_particle ~indent:i p in
  Printf.sprintf
    "%s{\n\
    %sparser = { name = %s };\n\
    %satoms = %s;\n\
    %smatched_groups = %s;\n\
    %ssubparticles = %s\n\
    %s}"
    (indent_str indent)
    (indent_str (indent + 1)) (bold_cyan p.parser.name)
    (indent_str (indent + 1)) (pp_list string_pp (indent + 1) p.atoms)
    (indent_str (indent + 1)) (print_opt_list string_pp (indent + 1) p.matched_groups)
    (indent_str (indent + 1)) (pp_list particle_pp (indent + 1) p.subparticles)
    (indent_str indent)[@@ocamlformat "disable"]

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

let indent_level (s : string) : int =
  let i = ref 0 in
  while !i < String.length s && s.[!i] = '\t' do
    incr i
  done;
  !i

(** [all_matching_groups s] is the list of all groups (including group 0, the
    entire match) from the most recent successful regex match on [s]. *)
let all_matching_groups (s : string) : string list =
  let rec aux i acc =
    try
      let g = Str.matched_group i s in
      aux (i + 1) (g :: acc)
    with _ -> List.rev acc
  in
  aux 0 []

let rec try_parsers (parsers : Parser.parser_def list) (line : string) =
  let open Str in
  match parsers with
  | ({ matching = `Cue cue; _ } as parser) :: rest
    when String.split_on_char ' ' line |> List.hd = cue ->
      `Cue parser
  | ({ matching = `Pattern pattern; _ } as parser) :: rest
    when string_match (regexp ("^" ^ pattern)) line 0 ->
      let groups = all_matching_groups line in
      `Pattern (parser, groups)
  | _ :: rest -> try_parsers rest line
  | [] -> `Fallback Parser.fallback_parser

let rec collect_indented_lines (lines : string list) (level : int) :
    string list * string list =
  match lines with
  | line :: rest when indent_level line >= level ->
      let unindented_line = String.sub line 1 (String.length line - 1) in
      let indented_lines, rest' = collect_indented_lines rest level in
      (unindented_line :: indented_lines, rest')
  | _ -> ([], lines)

let rec parse_document (reg : Parser.registry) (lines : string list) :
    particle list =
  match lines with
  | [] -> []
  | line :: rest when String.trim line = "" -> parse_document reg rest
  | line :: rest -> (
      let level = indent_level line in
      let indented_lines, rest' = collect_indented_lines rest (level + 1) in
      let atoms = String.split_on_char ' ' line in
      let collect_subparticles (parser : Parser.parser_def) : particle list =
        if parser.raw then [ make_raw_particle indented_lines ]
        else parse_document reg indented_lines
      in
      match try_parsers reg.parsers line with
      | `None -> parse_document reg rest'
      | `Cue parser ->
          {
            parser;
            atoms;
            matched_groups = None;
            subparticles = collect_subparticles parser;
          }
          :: parse_document reg rest'
      | `Fallback parser ->
          {
            parser;
            atoms = "*" :: atoms;
            matched_groups = None;
            subparticles = collect_subparticles parser;
          }
          :: parse_document reg rest'
      | `Pattern (parser, groups) ->
          {
            parser;
            atoms;
            matched_groups = Some groups;
            subparticles = collect_subparticles parser;
          }
          :: parse_document reg rest')

let evaluate_parser_value (p : particle) ~(content : string)
    ~(parent_html : string) (pval : Parser.parser_value) : string =
  match pval with
  | Parser.ReplaceString expr ->
      expr
      |> String.replace_all ~sub:"$name" ~by:p.parser.name
      |> String.replace_all ~sub:"$content" ~by:content
      |> String.replace_all ~sub:"$arg"
           ~by:(p.atoms |> List.tl |> String.concat " ")
      (* TODO: More *)
  | Parser.LuaFunction lua_func ->
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
          |}
          (particle_lua_self p) "nil" (escape_lua_str content)
          (escape_lua_str parent_html)
      in
      Lua_eval.eval_lua lua_func globals

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
        let replace_with =
          evaluate_parser_value part ~content:"" ~parent_html build_html
        in
        String.replace_first
          ~sub:(" " ^ to_replace ^ " ") (* Replace only entire atoms *)
          ~by:(" " ^ replace_with ^ " ")
          parent_html
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
        let html =
          match
            subparticles
            |> List.filter (fun s -> Option.is_some s.parser.aftertext)
          with
          | [] -> html
          | l -> evaluate_particles html l
        in
        html
  in
  List.map evaluate_particle particles |> String.concat " "

let parse input =
  let parsers_content =
    In_channel.with_open_bin "parsers.markup" In_channel.input_all
    |> String.split_on_char '\n'
  in
  let parsers = Parser.parse_parser_file parsers_content in
  parsers |> List.iter Parser.print_parser;
  print_endline "#######################################################";

  let reg : Parser.registry = { parsers } in

  let lines = String.split_on_char '\n' input in
  let document_particles = parse_document reg lines in
  document_particles |> List.map pp_particle |> List.iter print_endline;

  print_endline "\n#######################################################\n";
  evaluate_particles "" document_particles
(* match Parser.parse_parser_definition lines with *)
(* | None -> "" *)
(* | Some p -> *)
(*     Parser.print_parser p; *)
(*     "" *)
