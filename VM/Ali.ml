
open XNum;
open VM;
open Types;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;
open VMPrivate;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)

(*
value print_scope s = do
{
  Hashtbl.iter
    (fun n d -> do
      {
        Printf.printf "global %s =" (UString.to_string (symbol_to_string n));
        print_partial !d;
        Printf.printf "\n"
      })
    s.Scope.global_symbols;

  Hashtbl.iter
    (fun n (l, i) ->
      Printf.printf "local %s = <var %d %d>\n"
        (UString.to_string (symbol_to_string n))
        l i)
    s.Scope.local_symbols
};
*)

value main () = do
{
  Unicode.UString.set_string_format `UTF8;

  let debug = Array.fold_left (fun d arg -> d || arg = "--debug") False Sys.argv;

  if debug then
    !Evaluate.tracing_bytecode := True
  else ();

  print_string "This is ali, version 0.1.\nIf you need help, please type \"quit\".\n";
  flush stdout;

  try do
  {
    let scope = Primitives.initial_scope ();

    for i = 1 to Array.length Sys.argv - 1 do
    {
      if Sys.argv.(i) <> "--debug" then do
      {
        print_string "Loading ";
        print_string Sys.argv.(i);
        print_string "...";
        flush stdout;

        if debug then do
        {
          let code = Compile.compile_declarations scope (UCStream.of_file Sys.argv.(i));
          Evaluate.print_bytecode 3 code;
          flush stdout;
          ignore (Evaluate.execute code [ref Unbound])
        }
        else
          Machine.execute_declarations scope (UCStream.of_file Sys.argv.(i));

        print_string " done\n"
      }
      else ()
    };

    iter ()

    where rec iter () = do
    {
      print_string "> ";
      flush stdout;

      let expr = read_line ();

      if expr = "quit" then
        ()
      else do
      {
        try do
        {
          let result =
            if debug then do
            {
              let code = Compile.compile_expression scope (UCStream.of_list (UString.of_string expr));
              Evaluate.print_bytecode 3 code;
              flush stdout;
              let x = Evaluate.execute code [];
              !x;
            }
            else
              Machine.evaluate_expression scope (UCStream.of_list (UString.of_string expr));

          Evaluate.print_partial 5 result;
          print_newline ();
        }
        with
        [ Syntax_error (f,l,c) err ->
            Printf.printf "\n%s:%d:%d syntax error: %s\n" f l c (UString.to_string (Array.to_list err))
        | Runtime_error err -> Printf.printf "\nruntime error: %s\n" (UString.to_string (Array.to_list err))
        ];

        iter ()
      }
    };
  }
  with
  [ Syntax_error (f,l,c) err ->
      Printf.printf "\n%s:%d:%d syntax error: %s\n" f l c (UString.to_string (Array.to_list err))
  | Runtime_error err -> Printf.printf "\nruntime error: %s\n" (UString.to_string (Array.to_list err))
  ];

  flush stderr
};

main ();

