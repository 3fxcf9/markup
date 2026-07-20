let () =
  let input_file = Sys.argv.(1) in
  let content = In_channel.with_open_bin input_file In_channel.input_all in
  let html, metadata = Markup.parse content in
  print_endline html;
  metadata
  |> List.map (fun (key, value) -> Printf.sprintf "%s: %s" key value)
  |> String.concat "\n"
  |> Printf.printf "<!-- METADATA\n%s\n-->"

(* dune exec ./main/main.exe | ~/.local/share/nvim/mason/bin/prettier --parser html | bat --paging never *)
