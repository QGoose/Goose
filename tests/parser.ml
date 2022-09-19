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
  assert_parse
    "Parsing simple measurement without indices"
    parse_stmt
    "measure x -> y;"
    (Qop 
      (Q_measure
        ((A_id (Id "x", None))
        ,(A_id (Id "y", None)))))

let%test _ =
  assert_parse
    "Parsing arg without index"
    parse_arg
    "y"
    (A_id (Id "y", None))

let%test _ =
  assert_parse
    "Parsing arg with index"
    parse_arg
    "y[0]"
    (A_id (Id "y", Some (Nnint 0)))
    
let%test _ =
  assert_parse
    "Parsing index"
    parse_idx
    "[1]"
    (Nnint 1)

let%test _ =
  assert_parse
    "Parsing zero index"
    parse_idx
    "[0]"
    (Nnint 0)

let%test _ =
  assert_parse
    "Parsing measurement with single index"
    parse_stmt
    "measure x -> y[0];"
    (Qop 
      (Q_measure
        ((A_id (Id "x", None))
        ,(A_id (Id "y", Some (Nnint 0))))))

let%test _ =
  assert_parse
    "Parsing measurement with two indices"
    parse_stmt
    "measure x[0] -> y[1];"
    (Qop 
      (Q_measure
        ((A_id (Id "x", Some (Nnint 0)))
        ,(A_id (Id "y", Some (Nnint 1))))))
