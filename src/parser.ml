open Opal
open Qasm

let todo () = failwith "Unimplemented"

let id x = Id x
let nnint x = Nnint x

let non_zero =
  satisfy (function '1'..'9' -> true | _ -> false)

let parse_nnint =
  spaces
  >> (non_zero <~> many digit)
  => (implode % int_of_string % nnint)

let parse_id =
  spaces
  >> letter <~> many (alpha_num <|> exactly '_')
  => (implode % id)

let (let*) = (>>=)

let parse_idx =
  between (token "[") (token "]") parse_nnint

let parse_opt_idx =
  parse_idx => Option.some <|> return None

let parse_arg =
  let* id = parse_id in
  let* nn = parse_opt_idx in
  return (A_id (id, nn))

let parse_stmt =
  let qreg =
    let* _  = token "qreg" << space in
    let* id = parse_id in
    let* nn = parse_idx in
    let* _  = token ";" in
    return (Qreg (id, nn))
  in
  let creg =
    let* _  = token "creg" << space in
    let* id = parse_id in
    let* nn = parse_idx in
    let* _  = token ";" in
    return (Creg (id, nn))
  in
  let gop _ = todo ()
  in
  let gate =
    let* id = parse_id in
    let* l1 = many parse_id in
    let* l2 = many parse_id in
    let* l3 = between (token "{") (token "}") (many gop) in
    return (Gate (id, l1, l2, l3))
  in
  let uop _ = todo () in
  let measure =
    let* _ = token "measure" << space in
    let* a1 = parse_arg in
    let* _  = token "," in
    let* a2 = parse_arg in
    let* _  = token ";" in
    return (Q_measure (a1, a2))
  in
  let reset =
    let* _ = token "reset" << space in
    let* a = parse_arg in
    let* _  = token ";" in
    return (Q_reset a)
  in
  let qop  =
    choice [
      measure;
      reset;
      uop => (fun op -> Q_uop op)
    ]
  in
  let cond =
    let* _  = token "if" in
    let* _  = token "(" in
    let* id = parse_id in
    let* _  = token ")" in
    let* nn = parse_nnint in
    let* op = qop in
    return (If (id, nn, op))
  in
  let opaq =
    let* _  = token "opaque" << space in
    let* id = parse_id in
    let* l1 = many parse_id in
    let* l2 = many parse_id in
    let* _  = token ";" in
    return (Opaque (id, l1, l2))
  in
  let barr =
    let* _ = token "barrier" << space in
    let* l = many parse_arg in
    let* _ = token ";" in
    return (Barrier l)
  in
  choice [
    qreg;
    creg;
    gate;
    qop => (fun x -> Qop x);
    cond;
    opaq;
    barr
  ]


let parse_string s =
  LazyStream.of_string s |> parse_stmt