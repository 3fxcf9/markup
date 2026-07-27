open Lua_api
open Types

let getopt o = match o with Some v -> v | None -> raise Not_found

(* Custom functions exposed to lua *)
let read_file_from_disk ls =
  let path = LuaL.checkstring ls 1 in
  try
    let ic = open_in path in
    let length = in_channel_length ic in
    let contents = really_input_string ic length in
    close_in ic;

    Lua.pushstring ls contents;
    1
  with Sys_error err ->
    Lua.pushnil ls;
    Lua.pushstring ls err;
    2

let html_escape_lua ls =
  let s = LuaL.checkstring ls 1 in
  let escaped = html_escape s in
  Lua.pushstring ls escaped;
  1

let eval_lua (lua_func : string) (globals : string) =
  let ls = LuaL.newstate () in
  LuaL.openlibs ls;

  (* custom functions *)
  Lua.pushocamlfunction ls read_file_from_disk;
  Lua.setglobal ls "read_file";
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
