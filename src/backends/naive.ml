(** * A Naive Backend For Simulation of Quantum Circuits *)

open Simulation
open Utils

(* A simple quantum circuit simulation backend based on Qubit-Wise Multiplication. *)
module NaiveBackend = struct
  type qstate = Complex.t array

  let print_state (out: out_channel) (state: qstate) =
    let len = Array.length state in
    let print_cpx cpx =
      let { Complex.re; im; } = cpx in
      Printf.sprintf "%.3f + %.3fi (%.3f)" re im (Complex.norm2 cpx)
    in
      Array.iteri  (fun i s -> Printf.fprintf out "|%s> = %s\n" (int2bin i (log2 len)) (print_cpx s)) state
    
  (* Returns the indices of the state amplitudes required for the ith iteration of a gate on target t. *)
  let iteration_indices (i : int) (t : int) : int * int =
    let mask = (1 lsl t) - 1 in
    let notMask = lnot mask in
    let i1 = (i land mask) lor ((i land notMask) lsl 1) in
    let i2 = i1 lor (1 lsl t) in
    (i1, i2)

  (* Initialises a state vector given a number of qubits. *)
  let init qbits =
    let len = 1 lsl qbits in
    (* All-zero state *)
    let state = Array.make len Complex.zero in
    (* effectfully set the |00...0> entry to 1 *)
    Array.set state 0 Complex.one;
    state

  (* Check if an iteration should execute based on the controls of the gate. *)
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

  (* Returns the matrix corresponding to a gate. *)
  let matrix_for_gate (g : Circuit.gate_kind) : matrix =
    match g with
    | X -> Complex.(zero, one, one, zero)
    | Z -> Complex.(one, zero, zero, neg one)
    | H -> (cpx_inv_sqrt_2, cpx_inv_sqrt_2, cpx_inv_sqrt_2, Complex.neg cpx_inv_sqrt_2)
    | Rm m -> Complex.(one, zero, zero, exp (div (mul cpx_2_pi i) (cpx_pow_2 m)))
    | U { theta; phi; lambda; } -> Complex.(
        polar (cos (theta /. 2.)) (-. (phi +. lambda) /. 2.), polar (-. sin (theta /. 2.)) (-. (phi -. lambda) /. 2.),
        polar (sin (theta /. 2.)) ((phi -. lambda) /. 2.), polar (cos (theta /. 2.)) ((lambda +. phi) /. 2.)
      )

  (* Applies a gate to a state vector using QWM (2^(n-1) iterations). *)
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

