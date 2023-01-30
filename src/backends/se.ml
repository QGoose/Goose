(** A symbolic simulator for quantum circuits *)

open Symbolic
open Simulation
open Utils

module SEgraphBackend = struct 
  type qstate = int array
  type matrix = int * int * int * int

  let init qbits : qstate = 
    let len = 1 lsl qbits in
    let ids = Array.make len 0 in
    for i = 0 to len - 1 do
      ids.(i) <- Egraph.mk_var i
    done;
    ids

  let print_state (out: out_channel) (state: qstate) =
    let len = Array.length state in
    Array.iteri (fun i s -> 
      Printf.fprintf out "|%s> = " (int2bin i (log2 len));
      LExpr.print out (Egraph.repr s);
      Printf.fprintf out "\n"
    ) state      

  let cpx_0 = Egraph.mk_cst 0
  let cpx_1 = Egraph.mk_cst 1
  let cpx_2 = Egraph.mk_cst 2
  let cpx_pi = Egraph.register (EExpr.Epi)
  let cpx_i = Egraph.register (EExpr.Ei)
  let cpx_2_pi = Egraph.mk_mul cpx_2 cpx_pi
  let cpx_inv_sqrt_2 = Egraph.mk_custom "SQRT1_2"
  let cpx_pow_2 m = Egraph.mk_pow cpx_2 (Egraph.mk_cst m)
  let cpx_omega m = Egraph.mk_exp (Egraph.mk_div (Egraph.mk_mul cpx_2_pi cpx_i) (cpx_pow_2 m))

  let matrix_for_gate (g : Circuit.gate_kind) : matrix =
    match g with
    | X -> (cpx_0, cpx_1, cpx_1, cpx_0)
    | Z -> (cpx_1, cpx_0, cpx_0, Egraph.mk_neg cpx_1)
    | H -> (cpx_inv_sqrt_2, cpx_inv_sqrt_2, cpx_inv_sqrt_2, Egraph.mk_neg cpx_inv_sqrt_2)
    | Rm m -> (cpx_1, cpx_0, cpx_0, cpx_omega m)
    | U { theta = _theta; phi = _phi; lambda = _lambda; } -> Utils.todo ()
  
  (** Applies a gate to a state vector using symbolic QWM (2^(n-1) iterations). *)
  let apply_gate (g : Circuit.gate) (state : qstate) = 
    let iterations = Array.length state / 2 in
    let (a, b, c, d) = matrix_for_gate g.kind in
    let (A t) = g.target in

    for i = 0 to iterations - 1 do
      let (i1, i2) = iteration_indices i t in
      let s1 = state.(i1) in
      let s2 = state.(i2) in
      let check = controls_check i1 g.controls in
      if check then begin
        state.(i1) <- Egraph.(mk_add (mk_mul a s1) (mk_mul b s2));
        state.(i2) <- Egraph.(mk_add (mk_mul c s1) (mk_mul d s2));
      end
    done
  
end

(**
   A Symbolic backend to simulate quantum circuits
*)
module SBackend = struct
  type qstate = Expr.t array

  let print_state (out: out_channel) (state: qstate) =
    let len = Array.length state in
    Array.iteri  (fun i s -> Printf.fprintf out "|%s> = %s\n" (int2bin i (log2 len)) (Expr.repr s)) state   

  (** Initialises a state vector given a number of qubits. *)
  let init qbits =
    let len = 1 lsl qbits in
    (* All-zero state *)
    let state = Array.init len (fun i -> Expr.Var i) in
    (* let state = Array.make len (Expr.Cst (Complex.zero)) in *)
    (* effectfully set the |00...0> entry to 1 *)
    (* Array.set state 0 (Expr.Cst (Complex.one)); *)
    state

  type matrix = Expr.t * Expr.t * Expr.t * Expr.t

  let cpx_inv_sqrt_2 =
    Expr.Uop (INV, Uop (SQRT, Cst 2))

  let cpx_2_pi =
    Expr.(Cst 2 *! Pi)

  let cpx_pow_2 m =
    Expr.Bop (POW, Cst 2, Cst m)

  let cpx_omega m =
    Expr.(Uop (EXP, Bop (DIV, cpx_2_pi *! I, cpx_pow_2 m)))

  (** Returns the symbolic matrix corresponding to a gate kind. *)
  let matrix_for_gate (g : Circuit.gate_kind) : matrix =
    match g with
    | X -> Expr.(Cst 0, Cst 1, Cst 1, Cst 0)
    | Z -> Expr.(Cst 1, Cst 0, Cst 0, Cst (-1))
    | H -> Expr.(cpx_inv_sqrt_2, cpx_inv_sqrt_2, cpx_inv_sqrt_2, neg cpx_inv_sqrt_2)
    | Rm m -> Expr.(Cst 0, Cst 0, Cst 0, cpx_omega m)
    | U { theta = _theta; phi = _phi; lambda = _lambda; } -> Utils.todo ()

  (** Applies a gate to a state vector using symbolic QWM (2^(n-1) iterations). *)
  let apply_gate (g : Circuit.gate) (state : qstate) = 
    let iterations = Array.length state / 2 in
    let (a, b, c, d) = matrix_for_gate g.kind in
    let (A t) = g.target in

    for i = 0 to iterations - 1 do
      let (i1, i2) = iteration_indices i t in
      let c1 = state.(i1) in
      let c2 = state.(i2) in
      let check = controls_check i1 g.controls in
      if check then begin
        state.(i1) <- Expr.((a *! c1) +! (b *! c2));
        state.(i2) <- Expr.((c *! c1) +! (d *! c2));
      end
    done

  let repr x = x
end

(** Symbolic Simulator *)
module SE_Engine = Make(SBackend)

module SE_Egraph_Engine = Make(SEgraphBackend)