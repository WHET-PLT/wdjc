open Ast
open Sast
open Semcheck

module StringMap = Map.Make(String)

(*gets types from the semcheck*)
let string_of_vartype = function
   Int -> "int"
   | Note -> "note"
   | Chord -> "chord"
   | Track -> "track"
   | _ -> raise (Failure ("Illegal type"))

(*gets sast types*)
let ast_to_sast_type = function
   Ast.Int -> Sast.Int
   | Ast.Note -> Sast.Note
   | Ast.Chord -> Sast.Chord
   | Ast.Track -> Sast.Track
   | _ -> raise (Failure ("Mismatch type"))

