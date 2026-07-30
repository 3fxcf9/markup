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

(** [write_file path content] writes [content] into the file at [path] *)
let write_file path content =
  try
    Debug.log ~cat:IO "Writing file %s" path;
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    Ok ()
  with e ->
    Debug.log ~cat:IO "Error writing file: %s" (Printexc.to_string e);
    Error (Printf.sprintf "Failed to write file %s" path)

let rec create_dir_if_not_exists path =
  try
    if not (Sys.file_exists path) then begin
      ignore @@ create_dir_if_not_exists (Filename.dirname path);
      Sys.mkdir path 0o755
    end;
    Ok ()
  with Sys_error e -> Error (Printf.sprintf "System error: %s" e)

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
