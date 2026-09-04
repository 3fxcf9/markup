open Lua_api
open Types

let getopt o = match o with Some v -> v | None -> raise Not_found

let get_input_path reg path =
  Filename.concat reg.input_path path |> Fs.normalize_path

let get_output_path reg path =
  Filename.concat reg.output_path path |> Fs.normalize_path

(* Custom functions exposed to lua *)
let read_file_from_disk reg ls =
  let path = LuaL.checkstring ls 1 |> get_input_path reg in
  match Fs.read_file path with
  | Error _ ->
      Debug.error "Read error: file not found";
      Lua.pushnil ls;
      Lua.pushstring ls "File not found";
      2
  | Ok contents ->
      Lua.pushstring ls contents;
      1

let write_file_to_disk reg ls =
  let path = LuaL.checkstring ls 1 |> get_output_path reg in
  let contents = LuaL.checkstring ls 2 in
  match Fs.write_file path contents with
  | Ok () ->
      Lua.pushboolean ls true;
      1
  | Error err ->
      Debug.error "Write error: %s" err;
      Lua.pushboolean ls false;
      Lua.pushstring ls err;
      2

let copy_file_or_folder reg ls =
  let src = LuaL.checkstring ls 1 |> get_input_path reg in
  let dest = LuaL.checkstring ls 2 |> get_output_path reg in
  match Fs.copy src dest with
  | Ok () ->
      Lua.pushboolean ls true;
      1
  | Error err ->
      Debug.error "Copy error: %s" err;
      Lua.pushboolean ls false;
      1

let html_escape_lua ls =
  let s = LuaL.checkstring ls 1 in
  let escaped = Utils.html_escape s in
  Lua.pushstring ls escaped;
  1

let hash ls =
  let input = LuaL.checkstring ls 1 in
  input |> Hashtbl.hash |> string_of_int |> Lua.pushstring ls;
  1

(* Custom functions end *)

(** [ordered_records assoc key] is the list of values associated with [key] in
    [assoc], in the order they were added. *)
let ordered_records (assoc : (string * string) list) (key : string) :
    string list =
  assoc
  |> List.filter_map (fun (k, v) -> if k = key then Some v else None)
  |> List.rev

let distinct_keys (assoc : (string * string) list) : string list =
  List.fold_left
    (fun acc (k, _) -> if List.mem k acc then acc else k :: acc)
    [] assoc

let push_string_array ls (values : string list) : unit =
  Lua.newtable ls;
  List.iteri
    (fun i v ->
      Lua.pushstring ls v;
      Lua.rawseti ls (-2) (i + 1))
    values

(** [push_grouped_table ls assoc] pushes the association list [assoc] as a lua
    table whose keys are [distinct_keys assoc] and their value
    [ordered_records assoc key]. *)
let push_grouped_table ls (assoc : (string * string) list) : unit =
  Lua.newtable ls;
  distinct_keys assoc
  |> List.iter (fun key ->
      push_string_array ls (ordered_records assoc key);
      Lua.setfield ls (-2) key)

(* Lazy tables *)
let metadata_index reg ls =
  let key = LuaL.checkstring ls 2 in
  (match ordered_records !(reg.metadata) key with
  | [] -> Lua.pushnil ls
  | records -> push_string_array ls records);
  (* only push the requested metadata *)
  1

let external_metadata_index reg ls =
  let file = LuaL.checkstring ls 2 in
  (* function(self, key) *)
  (match List.assoc_opt file reg.external_metadata with
  | None -> Lua.pushnil ls
  | Some assoc -> push_grouped_table ls assoc);
  (* push only the metadata of the requested file *)
  1

(** [set_lazy_global ls name index_fn] sets the global [name] to an empty table
    whose reads are handled by [index_fn] via the [__index] metamethod (called
    for a non-existend key). *)
let set_lazy_global ls (name : string) (index_fn : Lua.state -> int) : unit =
  Lua.newtable ls;
  Lua.newtable ls;
  Lua.pushocamlfunction ls index_fn;
  Lua.setfield ls (-2) "__index";
  ignore (Lua.setmetatable ls (-2));
  Lua.setglobal ls name

let lua_error_css =
  {|<style>
.lua-error::before {
  content: "Lua error: ";
  color: red;
}
.lua-error {
  border: 2px solid red;
  background-color: rgba(255,0,0,0.2);
  border-radius: .5rem;
  padding: .5rem;
}
</style>|}
  |> String.replace_all ~sub:"\n" ~by:""

(** [run_chunk ?nresults ls code] loads and executes the Lua code [code] with no
    arguments, leaving [nresults] return values on the stack. *)
let run_chunk ?(nresults = 0) ls code =
  (match LuaL.loadstring ls code with
  | Lua.LUA_OK -> ()
  | _ -> failwith (getopt (Lua.tostring ls (-1))));
  match Lua.pcall ls 0 nresults 0 with
  | Lua.LUA_OK -> ()
  | _ -> failwith (getopt (Lua.tostring ls (-1)))

(** [get_state ?markup_parser reg] returns (and create if necessary) the Lua
    state, with the standard library opened and custom functions registered. *)
let get_state ?markup_parser (reg : registry) : Lua.state =
  match reg.lua_state with
  | Some ls -> ls
  | None ->
      let ls = LuaL.newstate () in
      LuaL.openlibs ls;

      Lua.pushocamlfunction ls (read_file_from_disk reg);
      Lua.setglobal ls "read_file";
      Lua.pushocamlfunction ls (write_file_to_disk reg);
      Lua.setglobal ls "write_file";
      Lua.pushocamlfunction ls (copy_file_or_folder reg);
      Lua.setglobal ls "fs_copy";
      Lua.pushocamlfunction ls html_escape_lua;
      Lua.setglobal ls "escape";
      Lua.pushocamlfunction ls hash;
      Lua.setglobal ls "hash";
      Lua.pushocamlfunction ls (fun ls ->
          LuaL.checkstring ls 1 |> Filename.dirname |> Lua.pushstring ls;
          1);
      Lua.setglobal ls "dirname";
      Lua.pushocamlfunction ls (fun ls ->
          LuaL.checkstring ls 1 |> Filename.basename |> Lua.pushstring ls;
          1);
      Lua.setglobal ls "basename";
      Lua.pushocamlfunction ls (fun ls ->
          LuaL.checkstring ls 1
          |> Filename.concat (Filename.dirname !(reg.file_path))
          |> Fs.normalize_path |> Lua.pushstring ls;
          1);
      Lua.setglobal ls "path_relative_to_toplevel";
      Option.iter
        (fun f ->
          Lua.pushocamlfunction ls f;
          Lua.setglobal ls "parse_markup")
        markup_parser;

      (* [metadata] and [external_metadata] are exposed to Lua as read-only tables with an [__index] metamethod, so only the key the lua code reads are uilt. This avoid parsing it on every evaluation (huge optimization for large projects). *)
      set_lazy_global ls "metadata" (metadata_index reg);
      set_lazy_global ls "external_metadata" (external_metadata_index reg);

      run_chunk ls
        {|
          find_subparticle = function (name)
              for _, p in ipairs(this.subparticles) do
                  if p.name == name then
                      return p
                  end
              end
              return nil
          end
        |};

      reg.lua_state <- Some ls;
      ls

let eval_lua ?markup_parser ~particle_file_path (reg : registry)
    (lua_func : string) (globals : string) =
  let ls = get_state ?markup_parser reg in

  Fun.protect
    ~finally:(fun () -> Lua.settop ls 0)
    (fun () ->
      try
        Lua.pushocamlfunction ls (fun ls ->
            LuaL.checkstring ls 1
            |> Filename.concat (Filename.dirname particle_file_path)
            |> Fs.normalize_path |> Lua.pushstring ls;
            1);
        Lua.setglobal ls "path_relative_to_file";
        run_chunk ls globals;
        run_chunk ~nresults:1 ls lua_func;

        match Lua.tostring ls (-1) with
        | Some s -> s
        | None -> failwith "Expected a string return value"
      with
      | Failure err ->
          Debug.log ~cat:Lua "ERROR: %s" err;
          Printf.sprintf {|%s<div class="lua-error">%s</div>|} lua_error_css err
      | _ -> "LUA_ERROR")
