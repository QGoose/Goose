open Symbolic
open Simulation
module SBackend : SBACKEND = struct
  type qstate = Expr.t array
  
  let iteration_indices (i : int) (t : int) : int * int =
    let mask = (1 lsl t) - 1 in
    let notMask = lnot mask in
    let i1 = (i land mask) lor ((i land notMask) lsl 1) in
    let i2 = i1 lor (1 lsl t) in
    (i1, i2)

  (* It depends on if we xant to allow custom inputs or not *)

  (* If we just assume that the initial state is a vector of zeros,
      then we just benefits of symbolic simplifications, but we don't need variables
      Yes but you get a symbolic result (an ast) instead of a concrete complex in the end
      so you can compile the ast to efficient c
      instead of doing the actual complex computation at (ocaml) runtime
  *)

  let init qbits =
    let len = 1 lsl qbits in
    (* All-zero state *)
    let state = Array.make len (Expr.Cst (Complex.zero)) in
    (* effectfully set the |00...0> entry to 1 *)
    Array.set state 0 (Expr.Cst (Complex.one));
    state

  let controls_check (state_index: int) (controls: Circuit.adr list): bool =
    let check (Circuit.A c) = (1 lsl c) land state_index > 0 in
    List.(fold_left (&&) true (map check controls))

  type matrix = Expr.t * Expr.t * Expr.t * Expr.t

  let cpx_2 =
    Complex.{ re = 2.0; im = 0.0 }

  let cpx_inv_sqrt_2 =
    Complex.(div one (sqrt cpx_2))

  let cpx_2_pi =
    Complex.{ re = 2.0 *. Float.pi; im = 0.0 }

  let cpx_pow_2 m =
    Complex.(pow cpx_2 { re = float_of_int m; im = 0.0 })

    
  let matrix_for_gate (g : Circuit.gate_kind) : matrix =
    match g with
    | X -> Expr.(Complex.(Cst zero, Cst one, Cst one, Cst zero))
    | Z -> Expr.(Complex.(Cst one, Cst zero, Cst zero, Cst (neg one)))
    | H -> Expr.(Complex.(Cst cpx_inv_sqrt_2, Cst cpx_inv_sqrt_2, Cst cpx_inv_sqrt_2, Cst (neg cpx_inv_sqrt_2)))
    | Rm m -> Expr.(Complex.(Cst one, Cst zero, Cst zero, Cst (exp (div (mul cpx_2_pi i) (cpx_pow_2 m)))))

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

(** Naive Simulator *)
module SE_Engine = Make(SBackend)
