open Lua_api
open Types

let getopt o = match o with Some v -> v | None -> raise Not_found

(* Custom functions exposed to lua *)
let read_file_from_disk reg ls =
  let path = LuaL.checkstring ls 1 in
  match reg.file_reader path with
  | None ->
      Lua.pushnil ls;
      Lua.pushstring ls "File not found";
      2
  | Some contents ->
      Lua.pushstring ls contents;
      1

let write_file_to_disk reg ls =
  let path = LuaL.checkstring ls 1 in
  let contents = LuaL.checkstring ls 2 in
  match reg.file_writer path contents with
  | Ok () ->
      Lua.pushboolean ls true;
      1
  | Error err ->
      Lua.pushboolean ls false;
      Lua.pushstring ls err;
      2

let html_escape_lua ls =
  let s = LuaL.checkstring ls 1 in
  let escaped = html_escape s in
  Lua.pushstring ls escaped;
  1

let eval_lua (reg : registry) (lua_func : string) (globals : string) =
  let ls = LuaL.newstate () in
  LuaL.openlibs ls;

  (* custom functions *)
  Lua.pushocamlfunction ls (read_file_from_disk reg);
  Lua.setglobal ls "read_file";
  Lua.pushocamlfunction ls (write_file_to_disk reg);
  Lua.setglobal ls "write_file";
  Lua.pushocamlfunction ls html_escape_lua;
  Lua.setglobal ls "escape";

  try
    (* load globals *)
    (match LuaL.loadstring ls globals with
    | Lua.LUA_OK -> ()
    | _ -> failwith (getopt (Lua.tostring ls (-1))));
    (* 0 arguments, 0 expected returns *)
    (match Lua.pcall ls 0 0 0 with
    | Lua.LUA_OK -> ()
    | _ -> failwith (getopt (Lua.tostring ls (-1))));

    (* lua_func evaluation *)
    (match LuaL.loadstring ls lua_func with
    | Lua.LUA_OK -> ()
    | _ -> failwith (getopt (Lua.tostring ls (-1))));
    (match Lua.pcall ls 0 1 0 with
    | Lua.LUA_OK -> ()
    | _ -> failwith (getopt (Lua.tostring ls (-1))));

    let result =
      match Lua.tostring ls (-1) with
      | Some s -> s
      | None -> failwith "Expected a string return value"
    in

    Lua.pop ls 1;

    result
  with
  | Failure err ->
      Debug.log ~cat:Lua "ERROR: %s" err;
      Printf.sprintf {|<div class="lua-error">%s</div>|} err
  | _ -> "LUA_ERROR"
