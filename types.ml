open Ast
open Sast
open Semcheck

module StringMap = Map.Make(String)

let string_of_type = function
   Int -> "int"
   | Note -> "note"
   | Chord -> "chord"
   | Track -> "track"
   | _ -> raise (Failure ("Illegal type"))


let sast_to_ast_type = function
   Ast.Int -> Sast.