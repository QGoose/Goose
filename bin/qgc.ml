open Quantumlib

let () =
  let nargs = Array.length Sys.argv in
  if nargs != 2 then
    (Printf.eprintf "Expected exactly one input file; found %i.\n" (nargs - 1); exit 2)
  else
    ();
  let filename = Sys.argv.(1) in
  let src_ic = open_in filename in
  let src = (try
               let src = really_input_string src_ic (in_channel_length src_ic) in
               close_in src_ic;
               src
             with e ->
               close_in_noerr src_ic;
               raise e;) in
  let ast = Parser.parse_ast src in
  let ir = Ir.lower_ast ast in
  let _ = Simulation.simulate ir in
  print_endline "goodbye";
