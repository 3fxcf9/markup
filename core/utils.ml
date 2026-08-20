(** [all_matching_groups s] is the list of all groups (including group 0, the
    entire match) from the most recent successful regex match on [s]. *)
let all_matching_groups (s : string) : string list =
  let rec aux i acc =
    try
      let g = Str.matched_group i s in
      aux (i + 1) (g :: acc)
    with _ -> List.rev acc
  in
  aux 0 []

let html_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b
