open Quantumlib
open Opal
open Parser

let assert_parse msg p i o =
  print_endline msg;
  match parse p (LazyStream.of_string i) with
  | None -> false
  | Some x -> x = o

let assert_fail msg p i =
  print_endline msg;
  match parse p (LazyStream.of_string i) with
  | None -> true
  | Some _ -> false

let%test _ =
  assert_parse
    "Parsing qreg 1"
    parse_stmt
    "qreg x[1];"
    (Qreg (Id "x", Nnint 1))

let%test _ =
  assert_parse
    "Parsing qreg 2"
    parse_stmt
    "qreg y[2];"
    (Qreg (Id "y", Nnint 2))

let%test _ =
  assert_parse
    "Parsing creg 1"
    parse_stmt
    "creg x[1];"
    (Creg (Id "x", Nnint 1))

let%test _ =
  assert_parse
    "Parsing creg 2"
    parse_stmt
    "creg y[2];"
    (Creg (Id "y", Nnint 2))

let%test _ =
  assert_fail
    "Parsing nnint leading zeros"
    parse_nnint
    "0001"