open Types

(* ---------- Pretty-printer ---------- *)

(* ---------- Main ---------- *)

let generate output files =
  let open Printf in
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

  List.iter (fun p -> fprintf oc "%s;\n" (pp_parser_def ~indent:2 p)) parsers;

  fprintf oc "]\n";

  close_out oc

let () =
  let output = Sys.argv.(1) in

  let files =
    Array.sub Sys.argv 2 (Array.length Sys.argv - 2) |> Array.to_list
  in

  generate output files
