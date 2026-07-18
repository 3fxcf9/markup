open String

type parser_value = ReplaceString of string | LuaFunction of string

type parser_def = {
  name : string;
  matching : [ `Cue of string | `Pattern of string ];
  aftertext : parser_value option;
      (* None or the text to replace in the parent node *)
  raw : bool;
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
    raw = false;
    arg_as_content = false;
  }

let raw_parser =
  {
    name = "raw";
    matching = `Cue "raw";
    aftertext = None;
    list_wrap = None;
    build_html = Some (ReplaceString "$arg");
    raw = false;
    arg_as_content = false;
    (* does not matter here *)
  }

type registry = { parsers : parser_def list }

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

let key_val_pattern = Str.regexp "\t\\([a-z_]+\\) \\(.+\\)"
let key_pattern = Str.regexp "\t\\([a-z_]+\\)"

let rec parse_parser_fields (lines : string list) : (string * string) list =
  match lines with
  | [] -> []
  | line :: rest ->
      let open Str in
      if string_match key_val_pattern line 0 then
        let key = matched_group 1 line in
        let value = matched_group 2 line in
        (key, value) :: parse_parser_fields rest
      else if string_match key_pattern line 0 then
        let value, lines_after = parse_indented_string rest in
        let key = matched_group 1 line in
        (key, value) :: parse_parser_fields lines_after
      else []

let parse_parser_definition (lines : string list) : parser_def option =
  match lines with
  | [] -> None
  | first_line :: rest -> (
      if not @@ starts_with ~prefix:"parser " first_line then None
      else
        let name = trim (sub first_line 7 (length first_line - 7)) in
        let fields = parse_parser_fields rest in
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
          | None, None -> None
        in
        let aftertext = get_value "aftertext" in
        let build_html = get_value "build_html" in
        let list_wrap = List.assoc_opt "list_wrap" fields in
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
        | None -> None
        | Some matching ->
            Some
              {
                name;
                matching;
                aftertext;
                list_wrap;
                build_html;
                raw;
                arg_as_content;
              })

let rec parse_parser_file (lines : string list) : parser_def list =
  match lines with
  | [] -> []
  | line :: rest when String.starts_with ~prefix:"parser" line -> (
      match parse_parser_definition lines with
      | None -> parse_parser_file rest
      | Some p -> p :: parse_parser_file rest)
  | _ :: rest -> parse_parser_file rest
