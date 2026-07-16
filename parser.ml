type parser_value = ReplaceString of string | LuaFunction of string

type parser_def = {
  name : string;
  matching : [ `Cue of string | `Pattern of string ];
  aftertext : parser_value option;
      (* None or the text to replace in the parent node *)
  list_wrap : string option;
  build_html : parser_value option;
}

let print_parser (parser : parser_def) =
  let print_parser_value_option key = function
    | None -> ()
    | Some (ReplaceString str) -> Printf.printf "%s: %s" key str
    | Some (LuaFunction str) ->
        Printf.printf "%s: \x1B[94m[LUA]\x1B[3;90m%s\x1B[0m" key str
  in
  Printf.printf "\x1B[1mPARSER %s\x1B[0m\n" parser.name;
  print_endline
    (match parser.matching with
    | `Cue cue -> "cue: " ^ cue
    | `Pattern p -> "pattern: " ^ p);
  print_parser_value_option "aftertext" parser.aftertext;
  print_parser_value_option "build_html" parser.build_html;
  match parser.list_wrap with
  | None -> ()
  | Some lw -> Printf.printf "list_wrap: %s\n" lw

let rec parse_indented_string (lines : string list) : string * string list =
  let open String in
  match lines with
  | line :: rest when starts_with ~prefix:"\t\t" line ->
      let content, lines_after = parse_indented_string rest in
      let unindented_line = sub line 2 (length line - 2) in
      (unindented_line ^ "\n" ^ content, lines_after)
  | line :: rest -> ("", line :: rest)
  | _ -> ("", [])

let rec parse_parser_fields (lines : string list) : (string * string) list =
  match lines with
  | [] -> []
  | line :: rest ->
      let open Str in
      let pattern = regexp "\t\\([a-z_]+\\) \\(.+\\)" in
      if string_match pattern line 0 then
        let key = matched_group 1 line in
        let value = matched_group 2 line in
        (key, value) :: parse_parser_fields rest
      else if string_match (regexp "\t\\([a-z_]+\\)") line 0 then
        match parse_indented_string rest with
        | "", _ -> []
        | value, lines_after ->
            let key = matched_group 1 line in
            (key, value) :: parse_parser_fields lines_after
      else []

let parse_parser_definition (lines : string list) : parser_def option =
  match lines with
  | [] -> None
  | first_line :: rest -> (
      let open String in
      if not @@ starts_with ~prefix:"parser " first_line then None
      else
        let name = trim (sub first_line 7 (length first_line - 7)) in
        let fields = parse_parser_fields rest in
        let parser_value_of_fields key =
          List.assoc_opt key fields
          |> Option.map (fun x ->
              if starts_with ~prefix:"function" x then LuaFunction x
              else ReplaceString x)
        in
        let matching_opt =
          match
            (List.assoc_opt "cue" fields, List.assoc_opt "pattern" fields)
          with
          | Some cue, _ -> Some (`Cue cue)
          | None, Some pattern -> Some (`Pattern pattern)
          | None, None -> None
        in
        let aftertext = parser_value_of_fields "aftertext" in
        let build_html = parser_value_of_fields "build_html" in
        let list_wrap = List.assoc_opt "list_wrap" fields in
        match matching_opt with
        | None -> None
        | Some matching ->
            Some { name; matching; aftertext; list_wrap; build_html })
