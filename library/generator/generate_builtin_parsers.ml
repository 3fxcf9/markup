open Printf
open Types

(* ---------- Pretty-printer ---------- *)

let pp_string oc s = fprintf oc "%S" s

let pp_option pp oc = function
  | None -> output_string oc "None"
  | Some x ->
      output_string oc "Some (";
      pp oc x;
      output_char oc ')'

let pp_parser_value oc = function
  | ReplaceString s -> fprintf oc "ReplaceString %S" s
  | LuaFunction s -> fprintf oc "LuaFunction %S" s

let pp_matching oc = function
  | `Cue s -> fprintf oc "`Cue %S" s
  | `Pattern s -> fprintf oc "`Pattern %S" s

let pp_parser_def oc p =
  fprintf oc "  {\n";
  fprintf oc "    name = %S;\n" p.name;

  fprintf oc "    matching = ";
  pp_matching oc p.matching;
  fprintf oc ";\n";

  fprintf oc "    aftertext = ";
  pp_option pp_parser_value oc p.aftertext;
  fprintf oc ";\n";

  fprintf oc "    raw = %b;\n" p.raw;

  fprintf oc "    head = %b;\n" p.head;

  fprintf oc "    arg_as_content = %b;\n" p.arg_as_content;

  fprintf oc "    list_wrap = ";
  pp_option pp_string oc p.list_wrap;
  fprintf oc ";\n";

  fprintf oc "    build_html = ";
  pp_option pp_parser_value oc p.build_html;
  fprintf oc ";\n";

  fprintf oc "    metadata = ";
  pp_option pp_parser_value oc p.metadata;
  fprintf oc ";\n";

  fprintf oc "  }"

(* ---------- Main ---------- *)

let generate output files =
  let parsers =
    files
    |> List.concat_map (fun file ->
        let content =
          In_channel.with_open_bin file In_channel.input_all
          |> String.split_on_char '\n'
        in
        Markup_parser.parse_parser_file content)
  in

  let oc = open_out output in

  fprintf oc "(* AUTO-GENERATED FILE. DO NOT EDIT. *)\n\n";
  fprintf oc "open Types\n\n";
  fprintf oc "let parsers = [\n";

  List.iter
    (fun p ->
      pp_parser_def oc p;
      fprintf oc ";\n")
    parsers;

  fprintf oc "]\n";

  close_out oc

let () =
  let output = Sys.argv.(1) in

  let files =
    Array.sub Sys.argv 2 (Array.length Sys.argv - 2) |> Array.to_list
  in

  generate output files
