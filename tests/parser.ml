open Quantumlib
open Opal
open Parser

let assert_parse msg p i o = 
  print_endline msg;
  match parse p i with
  | None -> false
  | Some x -> x = o

let assert_parse_fromString msg p i o =
  assert_parse msg p (LazyStream.of_string i) o

let read_file fn = LazyStream.of_channel (open_in fn)

let assert_parse_fromFile p fn o =
  assert_parse
   (String.concat "" ["Parsing `"; fn; "`"])
   p
   (read_file (String.concat "/" ["../src/tests/input"; fn]))
   o

let assert_fail msg p i =
  print_endline msg;
  match parse p (LazyStream.of_string i) with
  | None -> true
  | Some _ -> false

let%test _ =
  assert_parse_fromString
    "Parsing qreg 1"
    parse_stmt
    "qreg x[1];"
    (Qreg (Id "x", Nnint 1))

let%test _ =
  assert_parse_fromString
    "Parsing qreg 2"
    parse_stmt
    "qreg y[2];"
    (Qreg (Id "y", Nnint 2))

let%test _ =
  assert_parse_fromString
    "Parsing creg 1"
    parse_stmt
    "creg x[1];"
    (Creg (Id "x", Nnint 1))

let%test _ =
  assert_parse_fromString
    "Parsing creg 2"
    parse_stmt
    "creg y[2];"
    (Creg (Id "y", Nnint 2))

let%test _ =
  assert_parse_fromString
    "Parsing simple measurement without indices"
    parse_stmt
    "measure x -> y;"
    (Qop 
      (Q_measure
        ((A_id (Id "x", None))
        ,(A_id (Id "y", None)))))

let%test _ =
  assert_parse_fromString
    "Parsing arg without index"
    parse_arg
    "y"
    (A_id (Id "y", None))

let%test _ =
  assert_parse_fromString
    "Parsing arg with index"
    parse_arg
    "y[0]"
    (A_id (Id "y", Some (Nnint 0)))
    
let%test _ =
  assert_parse_fromString
    "Parsing index"
    parse_idx
    "[1]"
    (Nnint 1)

let%test _ =
  assert_parse_fromString
    "Parsing zero index"
    parse_idx
    "[0]"
    (Nnint 0)

let%test _ =
  assert_parse_fromString
    "Parsing measurement with single index"
    parse_stmt
    "measure x -> y[0];"
    (Qop 
      (Q_measure
        ((A_id (Id "x", None))
        ,(A_id (Id "y", Some (Nnint 0))))))

let%test _ =
  assert_parse_fromString
    "Parsing measurement with two indices"
    parse_stmt
    "measure x[0] -> y[1];"
    (Qop 
      (Q_measure
        ((A_id (Id "x", Some (Nnint 0)))
        ,(A_id (Id "y", Some (Nnint 1))))))

let%test _ =
  assert_fail
    "Parsing invalid measurement with three indices"
    parse_stmt
    "measure x[0] -> y[1] -> z[0];"

let%test _ =
  assert_fail
    "Invalid missing semicolon"
    parse_stmt
    (String.concat "\n"
      ["qreg x[1]"
      ;"qreg y[2]"
      ;"creg z[1]"
      ;"creg w[2]"
      ;"measure x -> y"
      ])

let%test _ =
  assert_parse_fromString
    "Apply a user-defined unitary gate"
    parse_stmt
    "crz(x) q[0];"
    (Qop (Q_uop
      (App ((Id "crz")
           ,[E_id (Id "x")]
           ,[A_id (Id "q", Some (Nnint 0))]))))
           
