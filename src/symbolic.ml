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
    | Cst of float
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
      Printf.sprintf "Cst %f" c
    | Pi -> Printf.sprintf "pi"
    | I -> Printf.sprintf "i"
    | Var v ->
      Printf.sprintf "Var %d" v
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

    | Bop (Qasm.MUL, Cst 0., _) -> Cst 0.
    | Bop (Qasm.MUL, _, Cst 0.) -> Cst 0.
    | Bop (Qasm.MUL, Cst 1., e) -> reduce e
    | Bop (Qasm.MUL, e, Cst 1.) -> reduce e
    | Bop (Qasm.ADD, Cst 0., e) -> reduce e
    | Bop (Qasm.ADD, e, Cst 0.) -> reduce e
    | Bop (Qasm.SUB, Cst 0., e) -> neg (reduce e)
    | Bop (Qasm.SUB, e, Cst 0.) -> reduce e
    | Bop (Qasm.DIV, Cst 0., _) -> Cst 0.
    | Bop (Qasm.DIV, e, Cst 1.) -> reduce e
    | Bop (Qasm.POW, Cst 0., _) -> Cst 0.
    | Bop (Qasm.POW, _, Cst 0.) -> Cst 1.
    | Bop (Qasm.POW, Cst 1., _) -> Cst 1.
    | Bop (Qasm.POW, e, Cst 1.) -> reduce e

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
    | Ecst of float
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

  let get (id : int) : EExpr.t = Hashtbl.find world id

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

  let repr_all () =
    world
    |> Hashtbl.to_seq
    |> List.of_seq
    |> List.sort (fun (x, _) (y, _) -> compare x y)
    |> List.map (fun (i, v) -> (i, EExpr.repr v))

  let is_mul (id : int) : (int * int) option = 
    match (get id) with
    | Ebop (Qasm.MUL, x1, x2) -> Some (x1, x2)
    | _ -> None

  let is_neg (id : int) : int option =
    match (get id) with
    | Euop (Qasm.NEG, x) -> Some x
    | _ -> None

  let is_sqrt (id : int) : int option = 
    match (get id) with
    | Euop (Qasm.SQRT, x) -> Some x
    | _ -> None

  let mk_cst c = register (Ecst c)
  let mk_var v = register (Evar v)
  let mk_custom s = register (Esym s)

  let rec mk_add x1 x2 =
    if x1 = x2 then mk_mul (mk_cst 2.) x1
    else if x1 = mk_cst 0. then x2
    else if x2 = mk_cst 0. then x1
    else match (is_mul x1, is_mul x2) with
      | Some (a, b), Some (c, d) when a = c -> mk_mul a (mk_add b d)
      | Some (a, b), Some (c, d) when a = d -> mk_mul a (mk_add b c)
      | Some (a, b), Some (c, d) when b = c -> mk_mul b (mk_add a d)
      | Some (a, b), Some (c, d) when b = d -> mk_mul b (mk_add a c)
      | _ -> register (Ebop (ADD, x1, x2))

  and mk_mul x1 x2 = 
    let c0 = mk_cst 0. in
    let c1 = mk_cst 1. in
    if x1 = x2 then mk_pow x1 (mk_cst 2.)
    else if x1 = c0 || x2 = c0 then c0
    else if x1 = c1 then x2 
    else if x2 = c1 then x1
    else register (Ebop (MUL, x1, x2))

  and mk_sub x1 x2 =
    let c0 = mk_cst 0. in
    if x1 = x2 then c0
    else if x1 = c0 then mk_neg x2
    else if x2 = c0 then x1
    else match (is_mul x1, is_mul x2) with
      | Some (a, b), Some (c, d) when a = c -> mk_mul a (mk_sub b d)
      | Some (a, b), Some (c, d) when a = d -> mk_mul a (mk_sub b c)
      | Some (a, b), Some (c, d) when b = c -> mk_mul b (mk_sub a d)
      | Some (a, b), Some (c, d) when b = d -> mk_mul b (mk_sub a c)
      | _ -> register (Ebop (SUB, x1, x2))

  and mk_div x1 x2 = 
    let c0 = mk_cst 0. in
    let c1 = mk_cst 1. in
    if x1 = c0 then c0
    else if x2 = c1 then x1
    else if x1 = c1 then mk_inv x2
    else register (Ebop (DIV, x1, x2))

  and mk_pow x1 x2 = 
    let c0 = mk_cst 0. in
    let c1 = mk_cst 1. in
    if x1 = c0 then c0 
    else if x2 = c0 then c1
    else if x1 = c1 then c1
    else if x2 = c1 then x1
    else register (Ebop (POW, x1, x2))

  and mk_sqrt x = register (Euop (SQRT, x))
  and mk_exp x = register (Euop (EXP, x))

  and mk_inv x =
    let c1 = mk_cst 1. in
    let c2 = mk_cst 2. in
    if x = c1 then c1 
    else match (is_sqrt x) with 
      | Some sqrted when sqrted = c2 -> mk_custom "SQRT1_2"
      | _ -> register (Euop (INV, x))

  and mk_neg x = 
    let c0 = mk_cst 0. in
    if x = c0 then c0
    else register (Euop (NEG, x))
end
