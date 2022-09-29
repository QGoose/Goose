(** * A Naive Backend For Simulation of Quantum Circuits *)

open Simulation

module NaiveBackend = struct
  type qstate = Complex.t array
  
  let iteration_indices (i : int) (t : int) : int * int =
    let mask = (1 lsl t) - 1 in
    let notMask = lnot mask in
    let i1 = (i land mask) lor ((i land notMask) lsl 1) in
    let i2 = i1 lor (1 lsl t) in
    (i1, i2)

  let init qbits =
    let len = 1 lsl qbits in
    (* All-zero state *)
    let state = Array.make len Complex.zero in
    (* effectfully set the |00...0> entry to 1 *)
    Array.set state 0 Complex.one;
    state

  let controls_check (state_index: int) (controls: Circuit.adr list): bool =
    let check (Circuit.A c) = (1 lsl c) land state_index > 0 in
    List.(fold_left (&&) true (map check controls))

  type matrix = Complex.t * Complex.t * Complex.t * Complex.t

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
    | X -> Complex.(zero, one, one, zero)
    | Z -> Complex.(one, zero, zero, neg one)
    | H -> (cpx_inv_sqrt_2, cpx_inv_sqrt_2, cpx_inv_sqrt_2, Complex.neg cpx_inv_sqrt_2)
    | Rm m -> Complex.(one, zero, zero, exp (div (mul cpx_2_pi i) (cpx_pow_2 m)))

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
        state.(i1) <- (Complex.add (Complex.mul a c1) (Complex.mul b c2));
        state.(i2) <- (Complex.add (Complex.mul c c1) (Complex.mul d c2))
      end
    done

  let repr x = x
end

(** Naive Simulator *)
module NaiveSimulator = Make(NaiveBackend)

