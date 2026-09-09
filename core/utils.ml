let hash =
  let open Types in
  Digest.string >> Digest.to_hex

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

let inner_html s =
  if String.length s = 0 || s.[0] <> '<' then s
  else
    match String.index_opt s '>' with
    | None -> s
    | Some i -> (
        match String.rindex_opt s '<' with
        | Some j when j > i && j + 1 < String.length s && s.[j + 1] = '/' ->
            String.sub s (i + 1) (j - i - 1)
        | _ -> s)
