let ( let* ) o f = match o with Error _ -> None | Ok x -> f x

let rec list_markup_files root path : string list =
  Fs.list_directory root path
  |> List.concat_map (function
    | `File f when Filename.check_suffix f ".markup" -> [ f ]
    | `Directory dir -> list_markup_files root dir
    | _ -> [])

let build_project input output http_root =
  assert (Sys.is_directory input) |> ignore;
  let markup_files = list_markup_files input "" in
  let external_metadata =
    markup_files
    |> List.map (fun f ->
        ( f |> Filename.remove_extension,
          Fs.read_file (Filename.concat input f)
          |> Markup.parse ~disable_external:true
          |> snd ))
  in
  markup_files
  |> List.iter (fun f ->
      let input_path = Filename.concat input f in
      let output_path =
        (Filename.concat output f |> Filename.chop_extension) ^ ".html"
      in
      if
        Filename.dirname output_path
        |> Fs.create_dir_if_not_exists |> Result.is_ok
      then
        Fs.read_file input_path
        |> Markup.parse ~external_metadata ~relative_path:(Filename.dirname f)
        |> fst |> Fs.write_file output_path |> ignore)
(* |> List.iter (fun (n, x) -> *)
(*     x *)
(*     |> List.map (fun (key, value) -> Printf.sprintf "%s: %s" key value) *)
(*     |> String.concat "\n" *)
(*     |> Printf.printf "METADATA [%s]\n%s\n\n" n) *)

let build_single_file path output http_root = ()
