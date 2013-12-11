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
    | Sast -> let program_t = Semcheck.sc_program program in 
          let listing = string_of_program_t in
          print_string listing
    (* | Java -> let listing = Compile.program_string (Semcheck.sc_program program)
          in print_endline listing *)
    (* | Java -> let listing = Compile.program_string program
              in print_endline listing
    | Compile -> Execute.javacompile (Compile.program_string (Semcheck.sc_program program)) *)