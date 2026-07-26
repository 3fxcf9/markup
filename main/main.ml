(* let () = *)
(*   let input_file = Sys.argv.(1) in *)
(*   let content = In_channel.with_open_bin input_file In_channel.input_all in *)
(*   let html, metadata = Markup.parse content in *)
(*   print_endline html; *)
(*   metadata *)
(*   |> List.map (fun (key, value) -> Printf.sprintf "%s: %s" key value) *)
(*   |> String.concat "\n" *)
(*   |> Printf.printf "<!-- METADATA\n%s\n-->" *)

(* dune exec ./main/main.exe | ~/.local/share/nvim/mason/bin/prettier --parser html | bat --paging never *)

open Cmdliner

let debug =
  let doc = "Print debug information to the console." in
  Arg.(value & flag & info [ "debug" ] ~doc)

let root =
  let doc =
    "Root URL for the generated site (eg. /repo-name/ for github pages)."
  in
  Arg.(value & opt string "/" & info [ "http_root" ] ~docv:"PATH" ~doc)

let input =
  let doc = "Markup file or directory." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

let output =
  let doc = "The directory to output to." in
  Arg.(value & pos 1 string "output" & info [] ~docv:"OUTPUT" ~doc)

let run debug http_root input output =
  Debug.set_enabled debug;
  ignore @@ Fs.create_dir_if_not_exists output;
  let ensure_slash_end path =
    if String.ends_with ~suffix:"/" path then path else path ^ "/"
  in
  let ensure_shash_beg path =
    if String.starts_with ~prefix:"/" path then path else "/" ^ path
  in
  let http_root = ensure_slash_end @@ ensure_shash_beg http_root in
  Debug.log "Using HTTP_ROOT=%s" http_root;
  if Sys.is_directory input then Ssg.build_project input output http_root
  else Ssg.build_single_file input output http_root

let cmd =
  let doc = "Parse markup files." in
  Cmd.v (Cmd.info "markup" ~doc)
    Term.(const run $ debug $ root $ input $ output)

let () = exit (Cmd.eval cmd)
