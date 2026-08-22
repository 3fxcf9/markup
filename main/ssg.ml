let rec list_markup_files root path : string list =
  Fs.list_directory root path
  |> List.concat_map (function
    | `File f when Filename.check_suffix f ".markup" -> [ f ]
    | `Directory dir -> list_markup_files root dir
    | _ -> [])

let build_project input output http_root =
  assert (Sys.is_directory input) |> ignore;
  assert (Sys.is_directory output) |> ignore;

  ignore @@ Fs.create_dir_if_not_exists output;

  let markup_files =
    list_markup_files input ""
    |> List.map (fun f -> (f, Fs.read_file (Filename.concat input f)))
  in

  let file_reader relative_path =
    Debug.log ~cat:IO "\027[95mFile reader called on file %s\027[0m"
      relative_path;
    let path = Filename.concat input relative_path in
    try Some (In_channel.with_open_bin path In_channel.input_all)
    with _ -> None
  in
  let file_writer relative_path contents =
    Debug.log ~cat:IO "\027[95mFile writer called on file %s\027[0m"
      relative_path;
    let path = Filename.concat output relative_path in
    Fs.write_file path contents
  in
  let fs_copy src dst =
    Debug.log ~cat:IO "\027[95mCopy called: %s -> %s\027[0m" src dst;
    let src = Filename.concat input src in
    let dst = Filename.concat output dst in
    Fs.copy src dst
  in

  (* Two pass approach. *)
  (* First pass: get the metadata for each file in the project. *)
  Debug.log "First pass (metadata)";
  let external_metadata =
    markup_files
    |> List.map (fun (name, content) ->
        ( name |> Filename.remove_extension,
          !(content
           |> Markup.parse ~relative_path:(Filename.dirname name)
                ~filename:(name |> Filename.basename |> Filename.chop_extension)
                ~file_reader ~file_writer ~fs_copy
           |> snd) ))
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
             ~relative_path:(Filename.dirname name)
             ~filename:(name |> Filename.basename |> Filename.chop_extension)
             ~file_reader ~file_writer ~fs_copy
        |> fst |> Fs.write_file output_path |> ignore)

let build_single_file input output http_root =
  assert (not (Sys.is_directory input)) |> ignore;

  let output_path =
    if Sys.file_exists output then
      if Sys.is_directory output then
        (input |> Filename.basename |> Filename.chop_extension
       |> Filename.concat output)
        ^ ".html"
      else output
    else if Filename.check_suffix output ".html" then (
      output |> Filename.dirname |> Fs.create_dir_if_not_exists |> ignore;
      output)
    else (
      Fs.create_dir_if_not_exists output |> ignore;
      (input |> Filename.basename |> Filename.chop_extension
     |> Filename.concat output)
      ^ ".html")
  in
  let file_reader relative_path =
    Debug.log ~cat:IO "File reader called on file %s" relative_path;
    let path = Filename.concat (Filename.dirname output_path) relative_path in
    try Some (In_channel.with_open_bin path In_channel.input_all)
    with _ -> None
  in
  let file_writer relative_path contents =
    Debug.log ~cat:IO "File writer called on file %s" relative_path;
    let path = Filename.concat (Filename.dirname output_path) relative_path in
    Fs.write_file path contents
  in

  Fs.read_file input
  |> Markup.parse ~file_reader ~file_writer
  |> fst |> Fs.write_file output_path |> ignore
