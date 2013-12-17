type action = Ast | Compile | Java | Sast

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ 
            ("-a", Ast);
            ("-s", Sast);
            ("-j", Java);
            ("-c", Compile) ]
  else Compile in
  
    let output_name = 
      if Array.length Sys.argv > 2 then
        Sys.argv.(2) 
      else "../DJ.mid" in
  
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
      Ast -> let listing = Ast.string_of_program program
          in print_string listing
    | Sast -> let program_t = Semcheck.sc_program program in 
          let listing = Sast.string_of_program_t program_t
          in print_string listing
    | Java -> let listing = Compile.string_of_program output_name (Semcheck.sc_program program)
          in ignore( listing );
    | Compile -> let listing = Compile.string_of_program output_name (Semcheck.sc_program program)
          in ignore( listing );
          let output = Sys.command("cd java; echo $PWD; make") in
            print_int output
              (* ignore(output); *)
(*           in ignore( listing ); *)
    (* | Java -> let listing = Compile.program_string program
              in print_endline listing
    | Compile -> Execute.javacompile (Compile.program_string (Semcheck.sc_program program)) *)