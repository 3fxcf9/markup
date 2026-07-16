let parse input =
  let lines = String.split_on_char '\n' input in
  match Parser.parse_parser_definition lines with
  | None -> ""
  | Some p ->
      Parser.print_parser p;
      ""
