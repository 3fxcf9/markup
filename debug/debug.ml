type category = General | Metadata | IO

let active_categories = ref [ General; Metadata; IO ]
let is_enabled cat = List.mem cat !active_categories
let enabled = ref true
let set_enabled b = enabled := b

let log ?(cat = General) fmt =
  if !enabled && is_enabled cat then
    let prefix =
      match cat with
      | General -> "\027[33m[DEBUG]\027[0m  "
      | Metadata -> "\027[34m[METADATA]\027[0m "
      | IO -> "\027[35m[I/O]\027[0m "
    in
    let prefix_fmt = Scanf.format_from_string prefix "" in
    Printf.kfprintf (fun _ -> ()) stderr (prefix_fmt ^^ fmt ^^ "\n%!")
  else Printf.ifprintf stderr fmt
