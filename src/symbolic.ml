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

let fresh =
  let count = ref (-1) in
  fun () -> incr count; !count

let mk_symbol i =
  Expr.CustomSymbol (Printf.sprintf "x_%d" i)

module EExpr = struct
  type t =
    | Ei
    | Epi
    | Evar of int
    | Ecst of int
    | Esym of string
    | Ebop of Qasm.binaryop * int * int
    | Euop of Qasm.unaryop * int

  (** Symbolic product. *)
  let ( *! ) x y = Ebop (Qasm.MUL, x, y)

  (** Symbolic sum. *)
  let ( +! ) x y = Ebop (Qasm.ADD, x, y)

  (** Symbolic negation. *)
  let neg x = Euop (Qasm.NEG, x)

  let repr (e : t) : Expr.t =
    match e with
    | Ei -> I
    | Epi -> Pi
    | Evar v -> Var v
    | Esym s -> CustomSymbol s
    | Ecst c -> Cst c
    | Ebop (op, i1, i2) -> Bop (op, mk_symbol i1, mk_symbol i2)
    | Euop (op, i) -> Uop (op, mk_symbol i)
end

module LExpr = struct
  type t = { bindings : (string * Expr.t) list; value : Expr.t }
  let print out { bindings; value } =
    List.iter (fun (x, e) ->
      Printf.fprintf out "let %s = %s in\n" x (Expr.repr e)
    ) bindings;
    Printf.printf "%s\n" (Expr.repr value)
end

module Egraph = struct
  type t = (int, EExpr.t) Hashtbl.t
  let world : t = Hashtbl.create 17

  let find_eexpr (e : EExpr.t) =
    Hashtbl.to_seq world
    |> Seq.find (fun (_, exp) -> exp = e)
    |> Option.map fst

  let register (e : EExpr.t) =
    match find_eexpr e with
    | Some id -> id
    | None ->
      let id = fresh () in
      Hashtbl.add world id e; id

  let repr (id : int) : LExpr.t =
    let rec collect acc (l : (int * EExpr.t) list) =
      match l with
      | [] -> assert false
      | (i, v)::_ when i = id -> LExpr.{ bindings = List.rev acc; value = EExpr.repr v }
      | (i, v)::l -> collect ((Printf.sprintf "x_%d" i, EExpr.repr v)::acc) l
    in
    world
    |> Hashtbl.to_seq
    |> List.of_seq
    |> List.sort (fun (x, _) (y, _) -> compare x y)
    |> collect []
end