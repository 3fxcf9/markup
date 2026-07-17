module T = Lua.Lib.Combine.T1 (Luaiolib.T)
module LuaioT = T.TV1
module C = Lua.Lib.Combine.C1 (Luaiolib.Make (LuaioT))
module I = Lua.MakeInterp (Lua.Parser.MakeStandard) (Lua.MakeEval (T) (C))

let eval_lua (lua_func : string) (globals : string) =
  try
    let state = I.mk () in
    print_endline globals;
    ignore (I.dostring state globals);
    let results = I.dostring state lua_func in
    let result =
      match results with
      | [ I.Value.LuaValueBase.String s ] -> s
      | _ -> failwith "Expected string"
    in
    result
  with
  | Failure err -> Printf.sprintf {|<div class="lua-error">%s</div>|} err
  | _ -> "LUA_ERROR"
