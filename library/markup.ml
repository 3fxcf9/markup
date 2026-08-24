let parse ?(disable_external = false) ?(external_metadata = [])
    ?(file_path = "") ?(input_path = ".") ?(output_path = ".") input =
  (* let parsers_content = *)
  (*   In_channel.with_open_bin "builtin_parsers.markup" In_channel.input_all *)
  (*   |> String.split_on_char '\n' *)
  (* in *)
  (* let parsers = Markup_parser.parse_parser_file parsers_content in *)
  (* (* parsers |> List.iter Parser.print_parser; *) *)
  (* (* print_endline "#######################################################"; *) *)
  (* let reg : Types.registry = { parsers } in *)

  (* Generate_builtin_parsers.generate "generated_parsers.ml" ["builtin_parsers/basic.markup"]; *)
  let parsers = Generated_builtin_parsers.parsers in
  let reg : Types.registry =
    {
      parsers = ref parsers;
      head = ref "";
      end_of_body = ref "";
      metadata = ref [];
      debug = ref false;
      external_metadata;
      file_path = ref file_path;
      depth = 0;
      lua_state = None;
      input_path;
      output_path;
    }
  in

  let lines = String.split_on_char '\n' input in
  let document_particles = Parser.parse_document reg lines in

  (* document_particles |> List.map pp_particle |> List.iter print_endline; *)
  (* print_endline "\n#######################################################\n"; *)
  let body, _ = Interpreter.evaluate_particles reg "" document_particles in
  ( Printf.sprintf
      {|<!DOCTYPE html><html><head><meta charset="UTF-8">%s</head><body>%s%s</body></html>|}
      !(reg.head) body !(reg.end_of_body),
    reg.metadata )
