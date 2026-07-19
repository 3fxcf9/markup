open String
open Types

let rec parse_indented_string ?(first = true) (lines : string list) :
    string * string list =
  match (lines, first) with
  | line :: rest, _ when starts_with ~prefix:"\t\t" line ->
      let content, lines_after = parse_indented_string rest ~first:false in
      let unindented_line = sub line 2 (length line - 2) in
      (unindented_line ^ "\n" ^ content, lines_after)
  (* default to flag *)
  | lines, true -> ("true", lines)
  | lines, _ -> ("", lines)

let key_val_pattern = Str.regexp "\t\\([a-z0-9_]+\\) \\(.+\\)"
let key_pattern = Str.regexp "\t\\([a-z0-9_]+\\)"

let rec parse_parser_fields (lines : string list) :
    (string * string) list * string list =
  match lines with
  | [] -> ([], [])
  | line :: rest ->
      let open Str in
      if string_match key_val_pattern line 0 then
        let key = matched_group 1 line in
        let value = matched_group 2 line in
        let assoc, rest' = parse_parser_fields rest in
        ((key, value) :: assoc, rest')
      else if string_match key_pattern line 0 then
        let value, lines_after = parse_indented_string rest in
        let key = matched_group 1 line in
        let assoc, rest' = parse_parser_fields lines_after in
        ((key, value) :: assoc, rest')
      else ([], rest)

let parse_parser_definition (lines : string list) :
    parser_def option * string list =
  match lines with
  | [] -> (None, [])
  | first_line :: rest ->
      begin if not @@ starts_with ~prefix:"parser " first_line then (None, lines)
      else
        let name = trim (sub first_line 7 (length first_line - 7)) in
        if
          String.length name <= 0
          || String.exists
               (function 'a' .. 'z' | '0' .. '9' | '_' -> false | _ -> true)
               name
        then (None, rest)
        else begin
          let fields, rest' = parse_parser_fields rest in
          let get_value key =
            List.assoc_opt key fields
            |> Option.map (fun x ->
                if starts_with ~prefix:"lua" x then
                  LuaFunction (sub x 3 (length x - 3))
                else ReplaceString x)
          in
          let matching_opt =
            match
              (List.assoc_opt "cue" fields, List.assoc_opt "pattern" fields)
            with
            | Some cue, _ -> Some (`Cue cue)
            | None, Some pattern -> Some (`Pattern pattern)
            | None, None -> Some (`Cue name)
          in
          let aftertext = get_value "aftertext" in
          let build_html = get_value "build_html" in
          let metadata = get_value "metadata" in
          let list_wrap = List.assoc_opt "list_wrap" fields in
          let head =
            match List.assoc_opt "head" fields with
            | Some "true" -> true
            | _ -> false
          in
          let raw =
            match List.assoc_opt "raw" fields with
            | Some "true" -> true
            | _ -> false
          in
          let arg_as_content =
            match List.assoc_opt "arg_as_content" fields with
            | Some "true" -> true
            | _ -> false
          in
          match matching_opt with
          | None -> (None, rest')
          | Some matching ->
              ( Some
                  {
                    name;
                    matching;
                    aftertext;
                    list_wrap;
                    build_html;
                    head;
                    raw;
                    metadata;
                    arg_as_content;
                  },
                rest' )
        end
      end

let rec parse_parser_file (lines : string list) : parser_def list =
  match lines with
  | [] -> []
  | line :: rest when String.starts_with ~prefix:"parser" line -> (
      match parse_parser_definition lines with
      | None, rest' -> parse_parser_file rest'
      | Some p, rest' -> p :: parse_parser_file rest')
  | _ :: rest -> parse_parser_file rest
