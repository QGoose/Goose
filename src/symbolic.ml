(** Symbolic computations.
    This module implements symbolic computations
    for complex-valued expressions.
    It is used by the symbolic simulator {!Se}.
*)

(** Manipulation of symbolic expressions. *)
module Expr = struct
  
  (** Type of symbolic expressions. *)
  type t =
    | Bop of Qasm.binaryop * t * t
    | Uop of Qasm.unaryop * t
    | Cst of int
    | Pi
    | I
    | Var of int
    | CustomSymbol of String.t

  (** Symbolic product. *)
  let ( *! ) x y = Bop (Qasm.MUL, x, y)
  
  (** Symbolic sum. *)
  let ( +! ) x y = Bop (Qasm.ADD, x, y)

  (** Symbolic negation. *)
  let neg x = Uop (Qasm.NEG, x)

  (** Returns a C template-compatible string of a binary operation. *)
  let cstring_of_binary op =
    match op with
    | Qasm.MUL -> "cmul"
    | Qasm.ADD -> "cadd"
    | Qasm.SUB -> "csub"
    | Qasm.DIV -> "cdiv"
    | Qasm.POW -> "cpow"

  (** Returns a C template-compatible string of a unary operation. *)
  let cstring_of_unary op = match op with
    | Qasm.SIN  -> "csin"
    | Qasm.COS  -> "ccos"
    | Qasm.TAN  -> "ctan"
    | Qasm.EXP  -> "cexp"
    | Qasm.NEG  -> "cneg"
    | Qasm.LN   -> "cln"
    | Qasm.SQRT -> "csqrt"
    | Qasm.INV  -> "cinv"

  (** Returns a string representation of the expression. *)
  let rec repr (e : t) =
    match e with
    | Bop (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (Qasm.string_of_binary op) (repr e1) (repr e2)
    | Uop (op, e) ->
      Printf.sprintf "(%s %s)" (Qasm.string_of_unary op) (repr e)
    | Cst c ->
      Printf.sprintf "%d" c
    | Pi -> Printf.sprintf "pi"
    | I -> Printf.sprintf "i"
    | Var v ->
      Printf.sprintf "%d" v
    | CustomSymbol s -> s

  (** Reduces an expression by matching against reduction rules. *)
  let rec reduce (e : t) = 
    match e with
    (* Distributivity *)
    | Bop (Qasm.ADD, Bop (Qasm.MUL, Cst c0, e0), Bop (Qasm.MUL, Cst c1, e1)) when c0 = c1 ->
      Bop (Qasm.MUL, Cst c0, Bop (Qasm.ADD, reduce e0, reduce e1))
    | Bop (Qasm.ADD, Bop (Qasm.MUL, CustomSymbol s0, e0), Bop (Qasm.MUL, CustomSymbol s1, e1)) when s0 = s1 ->
      Bop (Qasm.MUL, CustomSymbol s0, Bop (Qasm.ADD, reduce e0, reduce e1))
    (* Special cases for neg *)
    | Bop (Qasm.ADD, Bop (Qasm.MUL, Cst c0, e0), Bop (Qasm.MUL, Uop (Qasm.NEG, Cst c1), e1)) when c0 = c1 ->
      Bop (Qasm.MUL, Cst c0, Bop (Qasm.SUB, reduce e0, reduce e1))
    | Bop (Qasm.ADD, Bop (Qasm.MUL, CustomSymbol s0, e0), Bop (Qasm.MUL, Uop (Qasm.NEG, CustomSymbol s1), e1)) when s0 = s1 ->
      Bop (Qasm.MUL, CustomSymbol s0, Bop (Qasm.SUB, reduce e0, reduce e1))

    | Bop (Qasm.MUL, Cst 0, _) -> Cst 0
    | Bop (Qasm.MUL, _, Cst 0) -> Cst 0
    | Bop (Qasm.MUL, Cst 1, e) -> reduce e
    | Bop (Qasm.MUL, e, Cst 1) -> reduce e
    | Bop (Qasm.ADD, Cst 0, e) -> reduce e
    | Bop (Qasm.ADD, e, Cst 0) -> reduce e
    | Bop (Qasm.SUB, Cst 0, e) -> neg (reduce e)
    | Bop (Qasm.SUB, e, Cst 0) -> reduce e
    | Bop (Qasm.DIV, Cst 0, _) -> Cst 0
    | Bop (Qasm.DIV, e, Cst 1) -> reduce e
    | Bop (Qasm.POW, Cst 0, _) -> Cst 0
    | Bop (Qasm.POW, _, Cst 0) -> Cst 1
    | Bop (Qasm.POW, Cst 1, _) -> Cst 1
    | Bop (Qasm.POW, e, Cst 1) -> reduce e

    (* Base cases *)
    | Bop (op, e1, e2) -> Bop (op, reduce e1, reduce e2)
    | Uop (op, e) -> Uop (op, reduce e)
    | Cst c -> Cst c
    | Pi -> Pi
    | I -> I
    | Var v -> Var v
    | CustomSymbol s -> CustomSymbol s
end