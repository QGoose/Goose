(** Abstract Syntax Tree of OpenQASM *)

(** Type of Qasm programs *)
type t = {
  version : nnint * nnint;
  body : stmt list;
}

(** Type of Qasm statements *)
and stmt =
  (*  include statement *)
  | Include of string
  (* Quantum register *)
  | Qreg of id * nnint
  (* Classical register *)
  | Creg of id * nnint
  (* Gate declaration *)
  | GateDecl of gate_decl
  (* Quantum operator *)
  | Qop of qop
  (* Conditions *)
  | If of id * nnint * qop
  (* Opaque *)
  | Opaque of id * id list * id list
  (* Barrier *)
  | Barrier of arg list

and gate_decl = {
  name : id;
  params : id list;
  qargs : id list;
  gates : gop list;
}

(** Gate operators *)
and gop =
  | G_uop of uop
  | G_barrier of id list

(** Quantum operators *)
and qop =
  | Q_uop of uop
  | Q_measure of arg * arg
  | Q_reset of arg

and uop =
  | U of expr list * arg
  | CX of arg * arg
  | App of id * expr list * arg list

(** Identifiers *)
and id = Id of string

(** Non-negative integers *)
and nnint = Nnint of int

(* CM: documenting my assumptions as I write `compiler.ml`. *)
(* I'm assuming that the `id` corresponds to a register name, and the `nnint option` to an optional address. *)
(** Arguments of the operators *)
and arg =
  | A_id of id * nnint option

(** Expressions over reals *)
and expr =
  | E_cst of float
  | E_int of nnint
  | E_Pi
  | E_id of id
  | E_bop of binaryop * expr * expr
  | E_uop of unaryop * expr

(** Binary operations *)
and binaryop =
  | ADD
  | MUL
  | SUB
  | DIV
  | POW

(** Unary operations *)
and unaryop =
  | SIN
  | COS
  | TAN
  | EXP
  | NEG
  | LN
  | SQRT
  | INV

(** {2 Pretty printers}*)

let string_of_id = function
  | Id s -> s

let int_of_nnint = function
  | Nnint n -> n

let string_of_list ?(sep = ", ") (str : 'a -> string) (l : 'a list) =
  String.concat sep (List.map str l)

let rec string_of_stmt = function
  | Qreg (id, n) -> Printf.sprintf "qreg %s[%d];" (string_of_id id) (int_of_nnint n)
  | Creg (id, n) -> Printf.sprintf "creg %s[%d];" (string_of_id id) (int_of_nnint n)
  | GateDecl { name = Id name; params = params; qargs = qargs; gates = gates; } ->
    Printf.sprintf "%s (%s) %s {\n%s\n}" name
      (string_of_ids params) (string_of_ids qargs)
      (string_of_list ~sep:", " string_of_gop gates)
  | Qop q -> string_of_qop q
  | Include f -> Printf.sprintf "include \"%s\"" f
  | _ -> Utils.todo "qasm string_of_statement other case"

and string_of_qop (q : qop) =
  match q with
  | Q_uop u -> string_of_uop u
  | Q_measure (arg1, arg2) ->
    Printf.sprintf "measure %s, %s;" (string_of_arg arg1) (string_of_arg arg2)
  | Q_reset arg ->
    Printf.sprintf "reset %s;" (string_of_arg arg)

and string_of_uop (u : uop) =
  match u with
  | U (l, arg) ->
    Printf.sprintf "U (%s) %s;" (String.concat ", " (List.map string_of_expr l)) (string_of_arg arg)
  | CX (arg1, arg2) ->
    Printf.sprintf "CX %s, %s;" (string_of_arg arg1) (string_of_arg arg2)
  | App (Id name, exprs, args) ->
    Printf.sprintf "%s (%s) %s;" name
      (string_of_list string_of_expr exprs)
      (string_of_list string_of_arg args)

and string_of_gop (g : gop) =
  match g with
  | G_uop u -> string_of_uop u
  | G_barrier l -> Printf.sprintf "barrier %s;" (string_of_ids l)

and string_of_expr (e : expr) =
  match e with
  | E_cst f -> string_of_float f
  | E_int (Nnint i) -> string_of_int i
  | E_Pi -> "pi"
  | E_id (Id id) -> id
  | E_bop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_binary op) (string_of_expr e1) (string_of_expr e2)
  | E_uop (op, e) ->
    Printf.sprintf "%s(%s)" (string_of_unary op) (string_of_expr e)

and string_of_unary op =
  match op with
  | SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | NEG -> "neg"
  | LN -> "ln"
  | SQRT -> "√"
  | INV -> "1/"

and string_of_binary op =
  match op with
  | ADD -> "+"
  | MUL -> "*"
  | SUB -> "-"
  | DIV -> "/"
  | POW -> "^"

and string_of_arg (A_id (Id id, idx)) =
  match idx with
  | None -> id
  | Some (Nnint i) -> Printf.sprintf "%s[%d]" id i


and string_of_ids l =
  String.concat ", " (List.map (fun (Id x) -> x) l)

and string_of_stmts tab l =
  let l' = List.map string_of_stmt l in
  if tab then
    String.concat "\n" (List.map ((^) "  ") l')
  else
    String.concat "\n" l'

let string_of_qasm { version = (v1, v2); body } =
  String.concat "\n" [
    Printf.sprintf "OPENQASM %d.%d;" (int_of_nnint v1) (int_of_nnint v2);
    string_of_list ~sep:"\n" string_of_stmt body
  ]
