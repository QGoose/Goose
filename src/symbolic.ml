(** {1 Symbolic Computations} *)

module Symbol : sig
  type t
  val fresh : unit -> t
  val repr  : t -> string
end = struct
  type t = int

  let fresh =
    let current = ref 0 in
    fun () -> incr current; !current
  
  let repr i =
    Printf.sprintf "x_%d" i
end

module Expr = struct
  type t =
    | Bop of Qasm.binaryop * t * t
    | Uop of Qasm.unaryop * t
    | Cst of int
    | Pi
    | I
    | Var of Symbol.t

  let ( *! ) x y = Bop (Qasm.MUL, x, y)
  
  let ( +! ) x y = Bop (Qasm.ADD, x, y)

  let neg x = Uop (Qasm.NEG, x)
  
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
      Printf.sprintf "%s" (Symbol.repr v)
end