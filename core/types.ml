(* ---------- PARSER ---------- *)

type parser_value = ReplaceString of string | LuaFunction of string

type parser_def = {
  name : string;
  matching : [ `Cue of string | `Pattern of string ];
  aftertext : parser_value option;
      (* None or the text to replace in the parent node *)
  raw : bool;
  head : bool;
  metadata : parser_value option;
  arg_as_content : bool;
  list_wrap : string option;
  build_html : parser_value option;
}

let fallback_parser =
  {
    name = "paragraph";
    matching = `Cue "*";
    aftertext = None;
    list_wrap = None;
    build_html = Some (ReplaceString "<p>$arg</p>");
    head = false;
    raw = false;
    metadata = None;
    arg_as_content = false;
  }

let raw_parser =
  {
    name = "raw";
    matching = `Cue "raw";
    aftertext = None;
    list_wrap = None;
    build_html = Some (ReplaceString "$arg");
    head = false;
    raw = false;
    metadata = None;
    (* does not matter here *)
    arg_as_content = false;
  }

let print_parser (parser : parser_def) =
  let print_parser_value_option key = function
    | None -> ()
    | Some (ReplaceString str) -> Printf.printf "%s: %s\n" key str
    | Some (LuaFunction str) ->
        Printf.printf "%s: \x1B[94m[LUA]\x1B[3;90m%s\x1B[0m\n" key str
  in
  Printf.printf "\x1B[1mPARSER %s\x1B[0m\n" parser.name;
  print_endline
    (match parser.matching with
    | `Cue cue -> "cue: " ^ cue
    | `Pattern p -> "pattern: " ^ p);
  print_parser_value_option "aftertext" parser.aftertext;
  print_parser_value_option "build_html" parser.build_html;
  Option.iter (fun s -> Printf.printf "list_wrap: %s\n" s) parser.list_wrap;
  if parser.raw then print_endline "raw"

(* ---------- AST ---------- *)

type particle = {
  parser : parser_def;
  atoms : string list;
  matched_groups : string list option;
  subparticles : particle list;
}

let make_raw_particle (lines : string list) : particle =
  let raw = lines |> String.concat "\n" |> String.split_on_char ' ' in
  let cue = match raw_parser.matching with `Cue c -> c | _ -> "raw" in
  {
    parser = raw_parser;
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
      "[\n" ^ String.concat ";\n" items ^ "\n" ^ indent_str indent ^ "]"
  in
  let print_opt_list pp indent = function
    | None -> "None"
    | Some l -> pp_list pp indent l
  in
  let string_pp _ s = Printf.sprintf "%S" s in
  let particle_pp i p = pp_particle ~indent:i p in
  Printf.sprintf
    "{\n\
    %sparser = { name = %s };\n\
    %satoms = %s;\n\
    %smatched_groups = %s;\n\
    %ssubparticles = %s\n\
    %s}"
    (indent_str (indent + 1)) (bold_cyan p.parser.name)
    (indent_str (indent + 1)) (pp_list string_pp (indent + 1) p.atoms)
    (indent_str (indent + 1)) (print_opt_list string_pp (indent + 1) p.matched_groups)
    (indent_str (indent + 1)) (pp_list particle_pp (indent + 1) p.subparticles)
    (indent_str indent)[@@ocamlformat "disable"]

let print_particle (p : particle) = pp_particle p |> print_endline

(* ---------- REGISTRY ---------- *)

type registry = {
  mutable parsers : parser_def list;
  mutable head : string;
  mutable metadata : (string * string) list;
}
