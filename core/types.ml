let ( let* ) o f = match o with None -> None | Some x -> f x

(* ---------- PARSER ---------- *)
type parser_value = ReplaceString of string | LuaFunction of string

type parser_def = {
  name : string;
  matching : [ `Cue of string | `Pattern of string ];
  aftertext : [ `ParserValue of parser_value | `Pattern of string ] option;
      (* None or the text to replace in the parent node *)
  raw : bool;
  head : bool;
  metadata : parser_value option;
  arg_as_content : bool;
  list_wrap : string option;
  build_html : parser_value option;
  markup : string option;
}

let fallback_parser =
  {
    name = "paragraph";
    matching = `Cue "*";
    aftertext = None;
    list_wrap = None;
    (* escaped *)
    build_html = Some (ReplaceString "<p>$arg</p>");
    markup = None;
    head = false;
    raw = false;
    metadata = None;
    arg_as_content = false;
  }

let raw_parser =
  {
    name = "raw";
    matching = `Cue "";
    aftertext = None;
    list_wrap = None;
    (* unescaped *)
    (* build_html = Some (LuaFunction "return table.concat(this.atoms, ' ', 2)"); *)
    build_html = Some (ReplaceString "$content");
    markup = None;
    head = false;
    raw = false;
    metadata = None;
    (* does not matter here *)
    arg_as_content = false;
  }

let markup_file_include_parser =
  {
    name = "markup_include";
    matching = `Cue "";
    aftertext = None;
    list_wrap = None;
    build_html = Some (ReplaceString "$content");
    markup = None;
    head = false;
    raw = false;
    metadata = None;
    arg_as_content = false;
  }

let parser_debug_parser =
  let parser_debug_css =
    {|<style>
.parser-debug {
  background-color: rgba(0,0,255,0.2);
  border: 2px solid blue;
  border-radius: .5rem;
  margin-bottom: 1rem;
  .parser-name {
    border-bottom: 2px solid blue;
    padding: .5rem;
  }
  .parser-code {
    padding: .5rem;
    overflow: auto;
  }
}
</style>|}
    |> String.replace_all ~sub:"\n" ~by:""
  in
  {
    name = "parser_debug";
    matching = `Cue "";
    aftertext = None;
    list_wrap = None;
    build_html =
      Some
        (LuaFunction
           ({|return string.format('|} ^ parser_debug_css
          ^ {|<div class="parser-debug"><div class="parser-name">%s</div><div class="parser-code"><pre><code>%s</code></pre></div></div>', this.atoms[2], escape(this.content))|}
           ));
    markup = None;
    head = false;
    raw = false;
    metadata = None;
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
  let () =
    match parser.aftertext with
    | Some (`ParserValue pval) ->
        print_parser_value_option "aftertext" (Some pval)
    | Some (`Pattern pattern) ->
        Printf.printf "aftertext: \x1B[94m[PATTERN]\x1B[3;90m %s\n" pattern
    | None -> ()
  in
  print_parser_value_option "build_html" parser.build_html;
  Option.iter (fun s -> Printf.printf "list_wrap: %s\n" s) parser.list_wrap;
  if parser.raw then print_endline "raw"

let pp_string s = Printf.sprintf "%S" s
let pp_option pp = function None -> "None" | Some x -> "Some (" ^ pp x ^ ")"

let pp_parser_value = function
  | ReplaceString s -> Printf.sprintf "ReplaceString %S" s
  | LuaFunction s -> Printf.sprintf "LuaFunction %S" s

let pp_aftertext pp = function
  | `ParserValue s -> Printf.sprintf "`ParserValue (%s)" (pp s)
  | `Pattern s -> Printf.sprintf "`Pattern %S" s

let pp_matching = function
  | `Cue s -> Printf.sprintf "`Cue %S" s
  | `Pattern s -> Printf.sprintf "`Pattern %S" s

let pp_parser_def ?(indent = 0) p =
  let i = String.make indent ' ' in
  Printf.sprintf
    {|%s{
%s  name = %S;
%s  matching = %s;
%s  aftertext = %s;
%s  raw = %b;
%s  head = %b;
%s  arg_as_content = %b;
%s  list_wrap = %s;
%s  build_html = %s;
%s  markup = %s;
%s  metadata = %s;
%s}|}
    i
    i p.name
    i (pp_matching p.matching)
    i (pp_option (pp_aftertext pp_parser_value) p.aftertext)
    i p.raw
    i p.head
    i p.arg_as_content
    i (pp_option pp_string p.list_wrap)
    i (pp_option pp_parser_value p.build_html)
    i (pp_option pp_string p.markup)
    i (pp_option pp_parser_value p.metadata)
    i [@@ocamlformat "disable"]

(* ---------- AST ---------- *)

type particle = {
  parser : parser_def;
  atoms : string list;
  content : string;
  matched_groups : string list option;
  subparticles : particle list;
}

let make_raw_particle (lines : string list) : particle =
  {
    parser = raw_parser;
    atoms = "" :: [];
    content = String.concat "\n" lines;
    matched_groups = None;
    subparticles = [];
  }

let make_markup_include_particle (subparticles : particle list) : particle =
  {
    parser = markup_file_include_parser;
    atoms = "" :: [];
    content = "";
    matched_groups = None;
    subparticles;
  }

let make_parser_debug_particle (parser : parser_def) : particle =
  {
    parser = parser_debug_parser;
    atoms = [ ""; parser.name ];
    content = pp_parser_def parser;
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
  mutable depth : int;
  mutable debug : bool ref;
  mutable parsers : parser_def list ref;
  mutable head : string ref;
  mutable metadata : (string * string) list ref;
  external_metadata : (string * (string * string) list) list;
  mutable relative_path : string ref;
  mutable filename : string ref;
  file_reader : string -> string option;
  file_writer : string -> string -> (unit, string) result;
}
