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
    | Add of t * t
    | Mul of t * t
    | Neg of t
    | Cst of Complex.t
    | Var of Symbol.t

  let ( *! ) x y = Mul (x, y)
  
  let ( +! ) x y = Add (x, y)
  

  let neg x = Neg x
  
  let rec repr (e : t) =
    match e with
    | Add (e1, e2) ->
      Printf.sprintf "(add %s %s)" (repr e1) (repr e2)
    | Mul (e1, e2) ->
      Printf.sprintf "(mul %s %s)" (repr e1) (repr e2)
    | Neg e ->
      Printf.sprintf "(neg %s)" (repr e)
    | Cst c ->
      Printf.sprintf "%f + i%f" c.re c.im
    | Var v ->
      Printf.sprintf "%s" (Symbol.repr v)
end

module type SBACKEND = sig
  include Simulation.BACKEND
  val repr : qstate -> Expr.t array
end