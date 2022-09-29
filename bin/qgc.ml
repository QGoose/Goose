open Quantumlib

let () =
  let nargs = Array.length Sys.argv in
  if nargs != 2 then
    (Printf.eprintf "Expected exactly one input file; found %i.\n" (nargs - 1); exit 2)
  else
    ();
  let filename = Sys.argv.(1) in
  let ast = Parser.parse_file filename in
  let circ = Compiler.compile ast in
  let _res = Naive.NaiveSimulator.run circ in
  print_endline "goodbye";
