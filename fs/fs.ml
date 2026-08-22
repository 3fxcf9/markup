let list_directory root dir =
  (try
     let file_array = Sys.readdir (Filename.concat root dir) in
     Ok
       (Array.map
          (fun f ->
            let full = Filename.concat dir f in
            if Sys.is_directory (Filename.concat root full) then `Directory full
            else `File full)
          file_array)
   with Sys_error e -> Error e)
  |> Result.value ~default:[||] |> Array.to_list

let rec create_dir_if_not_exists path =
  try
    if not (Sys.file_exists path) then begin
      ignore @@ create_dir_if_not_exists (Filename.dirname path);
      Sys.mkdir path 0o755
    end;
    Ok ()
  with Sys_error e -> Error (Printf.sprintf "System error: %s" e)

(** [write_file path content] writes [content] into the file at [path]. Parent
    directories are created if necessary. *)
let write_file path content =
  try
    Debug.log ~cat:IO "Writing file %s" path;

    match create_dir_if_not_exists (Filename.dirname path) with
    | Error e -> Error e
    | Ok () ->
        let oc = open_out path in
        output_string oc content;
        close_out oc;
        Ok ()
  with e ->
    Debug.log ~cat:IO "Error writing file: %s" (Printexc.to_string e);
    Error (Printf.sprintf "Failed to write file %s" path)

let find_mde dir_path =
  try
    let files = Sys.readdir dir_path in
    Array.sort compare files;
    files
    |> Array.find_opt (fun file -> Filename.check_suffix file ".mde")
    |> Option.map (fun file -> Filename.concat dir_path file)
  with Sys_error _ -> None

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all with _ -> ""

let normalize_path path =
  let parts = String.split_on_char '/' path in
  let rec aux acc = function
    | [] -> List.rev acc
    | "" :: xs -> aux acc xs
    | "." :: xs -> aux acc xs
    | ".." :: xs ->
        begin match acc with
        | [] -> aux [ ".." ] xs
        | ".." :: _ -> aux (".." :: acc) xs
        | _ :: acc' -> aux acc' xs
        end
    | x :: xs -> aux (x :: acc) xs
  in
  String.concat "/" (aux [] parts)

let copy_file src dst = write_file dst (read_file src)

let rec copy src dest =
  try
    create_dir_if_not_exists dest |> ignore;

    if not (Sys.is_directory dest) then
      Error (Printf.sprintf "Destination is not a directory: %s" dest)
    else if not (Sys.file_exists src) then
      Error (Printf.sprintf "Source does not exist: %s" src)
    else
      let target = Filename.concat dest (Filename.basename src) in
      if Sys.is_directory src then begin
        if not (Sys.file_exists target) then Sys.mkdir target 0o755;

        Sys.readdir src |> Array.to_list
        |> List.fold_left
             (fun result name ->
               match result with
               | Error _ as e -> e
               | Ok () -> copy (Filename.concat src name) target)
             (Ok ())
      end
      else copy_file src target
  with Sys_error e -> Error (Printf.sprintf "System error: %s" e)
