(** OpenQASM parser (supports OpenQASM 2.0) *)

open Opal
open Qasm

let id x = Id x
let nnint x = Nnint x

let optional p = (p => Option.some) <|> return None

let non_zero =
  satisfy (function '1'..'9' -> true | _ -> false)

let singleton_zero = exactly '0' >> return ['0']

let parse_nnint =
  spaces
  >> (non_zero <~> many digit <|> singleton_zero)
     => (implode % int_of_string % nnint)

let parse_id =
  spaces
  >> letter <~> many (upper <|> alpha_num <|> exactly '_')
  => (implode % id)

let parse_file_name =
  let notquote = function '\"' -> false | _ -> true in
  spaces >> many1 (Opal.satisfy notquote) => implode

let (let*) = (>>=)

let parse_idx =
  between (token "[") (token "]") parse_nnint

let parse_opt_idx = optional parse_idx

let parse_arg =
  let* id = parse_id in
  let* nn = parse_opt_idx in
  return (A_id (id, nn))

let parens = between (token "(") (token ")")
let add = token "+" >> return (fun x y -> Qasm.E_bop (ADD, x, y))
let sub = token "-" >> return (fun x y -> Qasm.E_bop (SUB, x, y))
let mul = token "*" >> return (fun x y -> Qasm.E_bop (MUL, x, y))
let div = token "/" >> return (fun x y -> Qasm.E_bop (DIV, x, y))

let rec parse_expr input = chainl1 parse_term (add <|> sub) input
and parse_term input = chainl1 parse_factor (mul <|> div) input
and parse_factor input = (parens parse_expr <|> parse_atom) input
and parse_atom input = 
  choice [
    parens parse_expr;
    token "-" >> parse_atom => (fun x -> E_uop (NEG, x));
    token "ln" >> (parens parse_expr) => (fun x -> E_uop (LN, x));
    token "sin" >> (parens parse_expr) => (fun x -> E_uop (SIN, x));
    token "cos" >> (parens parse_expr) => (fun x -> E_uop (COS, x));
    token "tan" >> (parens parse_expr) => (fun x -> E_uop (TAN, x));
    token "exp" >> (parens parse_expr) => (fun x -> E_uop (EXP, x));
    token "sqrt" >> (parens parse_expr) => (fun x -> E_uop (SQRT, x));
    parse_const
  ] input
and parse_const input =
  choice [
    token "pi" >> return E_Pi;
    parse_id => (fun x -> E_id x);
    parse_nnint => (fun x -> E_int x);
  ] input

let parse_stmt =
  let inc =
    let* _ = token "include" << space in
    let* f = between (token "\"") (token "\"") parse_file_name in
    let* _ = token ";" in
    return (Include f)
  in
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

  let uop_u =
    let* _ = token "U" in
    let* l = between (token "(") (token ")") (sep_by parse_expr (token ",")) in
    let* a = parse_arg in
    let* _ = token ";" in
    return (U (l, a))
  in
  let uop_cx =
    let* _  = token "CX" in
    let* a1 = parse_arg in
    let* _  = token "," in
    let* a2 = parse_arg in
    let* _ = token ";" in
    return (CX (a1, a2))
  in
  let uop_app =
    let* id = parse_id in
    let* l1 = optional (between (token "(") (token ")") (sep_by parse_expr (token ","))) in
    let* l2 = sep_by1 parse_arg (token ",") in
    let* _ = token ";" in
    (* Hack: collapse a missing param list to an empty list *)
    let l1' = match l1 with
      | Some l -> l
      | None -> []
    in
    return (App (id, l1', l2))
  in
  let uop =
    choice [
      uop_u;
      uop_cx;
      uop_app;
    ]
  in
  let gop =
    choice [
      uop => (fun x -> G_uop x);
      token "barrier" >> (sep_by1 parse_id (token ",")) => (fun x -> G_barrier x)
    ]
  in
  let gate =
    let* _  = token "gate" in
    let* id = parse_id in
    let* l1 = optional (between (token "(") (token ")") (sep_by parse_id (token ","))) in
    let* l2 = sep_by1 parse_id (token ",") in
    let* l3 = between (token "{") (token "}") (many gop) in
    (* Hack: collapse a missing param list to an empty list *)
    let l1' = match l1 with
      | Some l -> l
      | None -> []
    in
    return (GateDecl {name = id; params = l1'; qargs = l2; gates = l3; })
  in
  let measure =
    let* _ = token "measure" << space in
    let* a1 = parse_arg in
    let* _  = token "->" in
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
    let* _  = token "==" in
    let* nn = parse_nnint in
    let* _  = token ")" in
    let* op = qop in
    return (If (id, nn, op))
  in
  let opaq =
    let* _  = token "opaque" << space in
    let* id = parse_id in
    let* l1 = sep_by parse_id (token ",") in
    let* l2 = sep_by1 parse_id (token ",") in
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
    inc;
    qreg;
    creg;
    qop => (fun x -> Qop x);
    barr;
    opaq;
    cond;
    gate
  ]


let parse_string s =
  LazyStream.of_string s |> parse_stmt

let parse_version =
  let* _ = token "OPENQASM" << space in
  let* _ = token "2.0" in
  let* _ = token ";" in
  return (Nnint 2, Nnint 0)

let parse_qasm =
  let* version = parse_version in
  let* ast = many1 parse_stmt in
  return {
    version = version;
    body = ast;
  }

let parse_lib_file filename =
  let src_ic = open_in filename in
  try
    let ast = Opal.parse (many1 parse_stmt) (LazyStream.of_channel src_ic) in
    close_in src_ic;
    match ast with
    | Some ast -> ast
    | None -> failwith (Printf.sprintf "Error while parsing %s" filename)
  with e ->
    close_in_noerr src_ic;
    raise e

module S = Set.Make(String)

let rec preprocess (visited : S.t) (l : stmt list) =
  List.concat_map (function
    | Include f ->
      if S.mem f visited then
        failwith (Printf.sprintf "Cyclic include statement %s" f)
      else
        preprocess (S.add f visited) (parse_lib_file f)
    | x -> [x]
  ) l

let parse_file filename =
  let src_ic = open_in filename in
  try
    let ast = Opal.parse parse_qasm (LazyStream.of_channel src_ic) in
    close_in src_ic;
    match ast with
    | Some { version; body } -> { version; body = preprocess S.empty body }
    | None -> failwith "didn't parse"
  with e ->
    close_in_noerr src_ic;
    raise e
