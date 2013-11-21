open Ast

module StringMap = Map.Make(String)

(*From compile as a place holder*)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

let string_of_type = function
   Int -> "int"
   | Note -> "note"
   | Chord -> "chord"
   | Track -> "track"