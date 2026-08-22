type category = General | Lua | IO | Parsing | Error

let active_categories = ref ([ General; Lua; IO; Parsing ] @ [ Error ])
let is_enabled cat = List.mem cat !active_categories
let enabled = ref true
let set_enabled b = enabled := b

let log ?(cat : category = General) fmt =
  if !enabled && is_enabled cat then
    let prefix =
      match cat with
      | General -> "\027[33m[DEBUG]\027[0m  "
      | Lua -> "\027[91m[LUA]\027[0m "
      | Parsing -> "\027[34m[PARSING]\027[0m "
      | IO -> "\027[35m[I/O]\027[0m "
      | Error -> "\027[31m[ERROR]\027[0m "
    in
    let prefix_fmt = Scanf.format_from_string prefix "" in
    Printf.kfprintf (fun _ -> ()) stderr (prefix_fmt ^^ fmt ^^ "\n%!")
  else Printf.ifprintf stderr fmt

let error fmt = log ~cat:Error fmt
