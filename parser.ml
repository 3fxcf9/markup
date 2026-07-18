open Types

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

let rec try_parsers (parsers : parser_def list) (line : string) =
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
  | [] -> `Fallback fallback_parser

let rec collect_indented_lines (lines : string list) (level : int) :
    string list * string list =
  match lines with
  | line :: rest when indent_level line >= level ->
      let unindented_line = String.sub line 1 (String.length line - 1) in
      let indented_lines, rest' = collect_indented_lines rest level in
      (unindented_line :: indented_lines, rest')
  | _ -> ([], lines)

let rec parse_document (reg : registry) (lines : string list) : particle list =
  match lines with
  | [] -> []
  | line :: rest when String.trim line = "" -> parse_document reg rest
  | line :: rest -> (
      let level = indent_level line in
      let indented_lines, rest' = collect_indented_lines rest (level + 1) in
      let atoms = String.split_on_char ' ' line in

      let collect_subparticles (parser : parser_def) :
          string list * particle list =
        let atoms, indented_lines =
          if parser.arg_as_content then begin
            ( [ List.hd atoms ],
              (atoms |> List.tl |> String.concat " ") :: indented_lines )
          end
          else (atoms, indented_lines)
        in

        if parser.raw then (atoms, [ make_raw_particle indented_lines ])
        else (atoms, parse_document reg indented_lines)
      in

      match try_parsers reg.parsers line with
      | `None -> parse_document reg rest'
      | `Cue parser ->
          let atoms, subparticles = collect_subparticles parser in
          { parser; atoms; matched_groups = None; subparticles }
          :: parse_document reg rest'
      | `Fallback parser ->
          let atoms, subparticles = collect_subparticles parser in
          { parser; atoms = "*" :: atoms; matched_groups = None; subparticles }
          :: parse_document reg rest'
      | `Pattern (parser, groups) ->
          let atoms, subparticles = collect_subparticles parser in
          { parser; atoms; matched_groups = Some groups; subparticles }
          :: parse_document reg rest')
