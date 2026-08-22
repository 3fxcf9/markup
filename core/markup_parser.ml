open String
open Types

let rec parse_indented_string ?(first = true) (lines : string list) :
    string * string list =
  match (lines, first) with
  | "" :: rest, _ | "\t\t" :: rest, _ ->
      let content, lines_after = parse_indented_string rest ~first in
      if content = "true" then (content, lines_after)
      else ("\n" ^ content, lines_after)
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
      if string_match key_val_pattern line 0 then (* key value *)
        let key = matched_group 1 line in
        let value = matched_group 2 line in
        let assoc, rest' = parse_parser_fields rest in
        ((key, value) :: assoc, rest')
      else if string_match key_pattern line 0 then (* key \n\t value *)
        let value, lines_after = parse_indented_string rest in
        let key = matched_group 1 line in
        let assoc, rest' = parse_parser_fields lines_after in
        ((key, value) :: assoc, rest')
      else ([], lines)

let parse_parser_definition ~(parsers : parser_def list) (lines : string list) :
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
          let matching =
            match
              (List.assoc_opt "cue" fields, List.assoc_opt "pattern" fields)
            with
            | Some cue, _ -> `Cue cue
            | None, Some pattern -> `Pattern pattern
            | None, None -> `Cue name
          in
          let aftertext =
            List.assoc_opt "aftertext" fields
            |> Option.map (fun x ->
                if starts_with ~prefix:"pattern" x then
                  `Pattern (sub x 8 (length x - 8))
                else if starts_with ~prefix:"lua" x then
                  `ParserValue (LuaFunction (sub x 4 (length x - 4)))
                else `ParserValue (ReplaceString x))
          in
          let build_html = get_value "build_html" in
          let markup = List.assoc_opt "markup" fields in
          let metadata = get_value "metadata" in
          let list_wrap = List.assoc_opt "list_wrap" fields in
          let head =
            match List.assoc_opt "head" fields with
            | Some "true" -> true
            | _ -> false
          in
          let end_of_body =
            match List.assoc_opt "end_of_body" fields with
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

          let parser =
            let parent =
              let* parser_name = List.assoc_opt "extends" fields in
              List.find_opt (fun p -> p.name = parser_name) parsers
            in
            match parent with
            | None ->
                {
                  name;
                  matching;
                  aftertext;
                  list_wrap;
                  build_html;
                  markup;
                  head;
                  end_of_body;
                  raw;
                  metadata;
                  arg_as_content;
                }
            | Some p ->
                let ( ||* ) x default =
                  match x with Some _ -> x | None -> default
                in
                {
                  name;
                  matching;
                  aftertext = aftertext ||* p.aftertext;
                  list_wrap = list_wrap ||* p.list_wrap;
                  build_html = build_html ||* p.build_html;
                  markup = markup ||* p.markup;
                  head = head || p.head;
                  end_of_body = end_of_body || p.end_of_body;
                  raw = raw || p.raw;
                  metadata = metadata ||* p.metadata;
                  arg_as_content = arg_as_content || p.arg_as_content;
                }
          in

          (Some parser, rest')
        end
      end

let rec parse_parser_file ?(acc = []) (lines : string list) : parser_def list =
  match lines with
  | [] -> List.rev acc
  | line :: rest when String.starts_with ~prefix:"parser " line -> (
      match parse_parser_definition ~parsers:acc lines with
      | None, rest' -> parse_parser_file ~acc rest'
      | Some p, rest' -> parse_parser_file ~acc:(p :: acc) rest')
  | _ :: rest -> parse_parser_file ~acc rest
