let ( let* ) o f = match o with Error _ -> None | Ok x -> f x

let rec list_markup_files root path : string list =
  Fs.list_directory root path
  |> List.concat_map (function
    | `File f when Filename.check_suffix f ".markup" -> [ f ]
    | `Directory dir -> list_markup_files root dir
    | _ -> [])

let build_project input output http_root =
  assert (Sys.is_directory input) |> ignore;
  let markup_files =
    list_markup_files input ""
    |> List.map (fun f -> (f, Fs.read_file (Filename.concat input f)))
  in
  (* Two pass approach. *)
  (* First pass: get the metadata for each file in the project. *)
  Debug.log "First pass (metadata)";
  let external_metadata =
    markup_files
    |> List.map (fun (name, content) ->
        ( name |> Filename.remove_extension,
          content
          |> Markup.parse ~disable_external:false
               ~relative_path:(Filename.dirname name)
               ~project_files:markup_files
          |> snd ))
    (* FIXME *)
  in

  (* external_metadata *)
  (* |> List.iter (fun (n, x) -> *)
  (*     x *)
  (*     |> List.map (fun (key, value) -> Printf.sprintf "%s: %s" key value) *)
  (*     |> String.concat "\n" *)
  (*     |> Printf.printf "METADATA [%s]\n%s\n\n" n); *)

  (* Second pass: actual render *)
  Debug.log "Rendering";
  markup_files
  |> List.filter (fun (name, _) ->
      not (Filename.check_suffix name ".lib.markup"))
  |> List.iter (fun (name, content) ->
      let output_path =
        (Filename.concat output name |> Filename.chop_extension) ^ ".html"
      in
      if
        Filename.dirname output_path
        |> Fs.create_dir_if_not_exists |> Result.is_ok
      then
        content
        |> Markup.parse ~external_metadata
             ~relative_path:(Filename.dirname name) ~project_files:markup_files
        |> fst |> Fs.write_file output_path |> ignore)

let build_single_file path output http_root = ()
