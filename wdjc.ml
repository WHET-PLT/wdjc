type action = Ast | Compile | Java | Sast

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
            ("-s", Sast);
            (* ("-c", Compile); *)
            ("-j", Java) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
  	  Ast -> let listing = Ast.string_of_program program
          in print_string listing
    | Java -> let listing = Compile.program_string (Semcheck.translate program)
          in print_endline listing
    | Compile -> Execute.javacompile (Compile.program_string (Semcheck.translate program))