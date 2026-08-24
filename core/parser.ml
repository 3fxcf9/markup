open Types

let indent_level (s : string) : int =
  let i = ref 0 in
  while !i < String.length s && s.[!i] = '\t' do
    incr i
  done;
  !i

let rec try_parsers (parsers : parser_def list) (line : string) =
  let open Str in
  match parsers with
  | ({ matching = `Cue cue; _ } as parser) :: rest
    when String.split_on_char ' ' line |> List.hd = cue ->
      `Cue parser
  | ({ matching = `Pattern pattern; _ } as parser) :: rest
    when string_match (regexp ("^" ^ pattern)) line 0 ->
      let groups = Utils.all_matching_groups line in
      `Pattern (parser, groups)
  | _ :: rest -> try_parsers rest line
  | [] -> `Fallback fallback_parser

let rec collect_indented_lines (lines : string list) (level : int) :
    string list * string list =
  match lines with
  | "" :: rest ->
      let indented_lines, rest' = collect_indented_lines rest level in
      ("\n" :: indented_lines, rest')
  | line :: rest when indent_level line >= level ->
      let unindented_line = String.sub line 1 (String.length line - 1) in
      let indented_lines, rest' = collect_indented_lines rest level in
      (unindented_line :: indented_lines, rest')
  | _ -> ([], lines)

let path_relative_to_root (reg : registry) (path : string) : string =
  (if String.starts_with ~prefix:"/" path then path
   else Filename.concat (Filename.dirname !(reg.file_path)) path)
  |> Fs.normalize_path

let resolve_path (reg : registry) (path : string) : string =
  let relative_to_root =
    if String.starts_with ~prefix:"/" path then path
    else Filename.concat (Filename.dirname !(reg.file_path)) path
  in
  Filename.concat reg.input_path relative_to_root |> Fs.normalize_path

let rec parse_document (reg : registry) (lines : string list) : particle list =
  if reg.depth >= 100 then
    failwith "Maximum dependency depth exceeded, probable cycle."
  else
    match lines with
    | [] -> []
    | line :: rest when String.trim line = "" -> parse_document reg rest
    | "debug on" :: rest ->
        reg.debug := true;
        parse_document reg rest
    | "debug off" :: rest ->
        reg.debug := false;
        parse_document reg rest
    | line :: rest
      when Filename.check_suffix line ".markup"
           && resolve_path reg line |> Fs.read_file |> Result.is_ok ->
        let relative_to_root = path_relative_to_root reg line in
        Debug.log ~cat:Parsing "Markup file inclusion: %s" relative_to_root;
        let content =
          resolve_path reg line |> Fs.read_file |> Result.get_ok
          |> String.split_on_char '\n'
        in
        let old_path = reg.file_path in
        reg.file_path <- ref relative_to_root;
        reg.depth <- reg.depth + 1;
        let included_particles =
          make_markup_include_particle (parse_document reg content)
        in
        reg.file_path <- old_path;
        reg.depth <- reg.depth - 1;
        included_particles :: parse_document reg rest
    | line :: rest ->
        if String.starts_with ~prefix:"parser " line then
          match
            Markup_parser.parse_parser_definition ~parsers:!(reg.parsers) lines
          with
          | None, rest' -> parse_document reg rest'
          | Some p, rest' -> (
              reg.parsers := !(reg.parsers) @ [ p ];
              match !(reg.debug) with
              | true -> make_parser_debug_particle p :: parse_document reg rest'
              | false -> parse_document reg rest')
        else begin
          let level = indent_level line in
          let indented_lines, rest' = collect_indented_lines rest (level + 1) in
          let atoms = String.split_on_char ' ' line in

          let collect_subparticles (parser : parser_def) :
              string list * particle list =
            let atoms, indented_lines =
              if parser.arg_as_content then begin
                (* ( [ List.hd atoms ], *)
                ( atoms,
                  (atoms |> List.tl |> String.concat " ") :: indented_lines )
              end
              else (atoms, indented_lines)
            in

            let indented_lines =
              match parser with
              | { name = "paragraph"; _ } -> "inline_markup" :: indented_lines
              | _ -> indented_lines
            in

            if parser.raw then (atoms, [ make_raw_particle indented_lines ])
            else (atoms, parse_document reg indented_lines)
          in

          match try_parsers !(reg.parsers) line with
          | `None -> parse_document reg rest'
          | `Cue { markup = Some m; _ } ->
              let new_lines = String.split_on_char '\n' m in
              parse_document reg (new_lines @ rest')
          | `Cue parser ->
              let atoms, subparticles = collect_subparticles parser in
              {
                parser;
                atoms;
                content = "";
                matched_groups = None;
                subparticles;
                file_path = !(reg.file_path);
              }
              :: parse_document reg rest'
          | `Fallback parser ->
              let atoms, subparticles = collect_subparticles parser in
              {
                parser;
                atoms = "*" :: atoms;
                content = "";
                matched_groups = None;
                subparticles;
                file_path = !(reg.file_path);
              }
              :: parse_document reg rest'
          | `Pattern ({ markup = Some m; _ }, _) ->
              let new_lines = String.split_on_char '\n' m in
              parse_document reg (new_lines @ rest')
          | `Pattern (parser, groups) ->
              let atoms, subparticles = collect_subparticles parser in
              {
                parser;
                atoms;
                content = "";
                matched_groups = Some groups;
                subparticles;
                file_path = !(reg.file_path);
              }
              :: parse_document reg rest'
        end
