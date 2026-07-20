let parse input =
  (* let parsers_content = *)
  (*   In_channel.with_open_bin "builtin_parsers.markup" In_channel.input_all *)
  (*   |> String.split_on_char '\n' *)
  (* in *)
  (* let parsers = Markup_parser.parse_parser_file parsers_content in *)
  (**)
  (* (* parsers |> List.iter Parser.print_parser; *) *)
  (* (* print_endline "#######################################################"; *) *)
  (* let reg : Types.registry = { parsers } in *)

  (* Generate_builtin_parsers.generate "generated_parsers.ml" ["builtin_parsers/basic.markup"]; *)
  let parsers =
      Generated_builtin_parsers.parsers in
  let reg: Types.registry = {parsers; head=""; metadata=[]; debug=false} in

  let lines = String.split_on_char '\n' input in
  let document_particles = Parser.parse_document reg lines in

  (* document_particles |> List.map pp_particle |> List.iter print_endline; *)
  (* print_endline "\n#######################################################\n"; *)
  let body = Interpreter.evaluate_particles reg "" document_particles in
  (Printf.sprintf {|<!DOCTYPE html><head>%s</head><body>%s</body>|} reg.head body), reg.metadata
