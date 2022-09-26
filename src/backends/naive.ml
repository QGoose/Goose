(** * A Naive Backend For Simulation of Quantum Circuits *)

open Simulation

let complex_two = Complex.{re = 2.0; im = 0.0}
let one_over_sqrt_2 = Complex.(div one (sqrt complex_two))
let two_pi = Complex.{re = 2.0 *. Float.pi; im = 0.0}

module NaiveBackend : BACKEND with type qstate = Complex.t array = struct
  type qstate = Complex.t array
  
  let iterationIndices : int -> int -> int * int = fun i t ->
    let mask = (1 lsl t) - 1 in
    let notMask = lnot mask in
    let firstIndex = (i land mask) lor ((i land notMask) lsl 1) in
    let secondIndex = (firstIndex) lor (1 lsl t) in
    (firstIndex, secondIndex) 

  let init qbits =
    let len = 1 lsl qbits in
    (* All-zero state *)
    let state = Array.make len Complex.zero in
    (* effectfully set the |00...0> entry to 1 *)
    Array.set state 0 Complex.one;
    state

  let controls_check (state_index: int) (controls: Circuit.adr list): bool = 
    List.fold_left (&&) true (List.map (fun (Circuit.A c) -> ((1 lsl c) land state_index) > 0) controls)
    
  let matrix_for_gate (g: Circuit.gate_kind): Complex.t * Complex.t * Complex.t * Complex.t = match g with
    | X -> Complex.(zero, one, one, zero)
    | Z -> Complex.(one, zero, zero, neg one)
    | H -> (one_over_sqrt_2, one_over_sqrt_2, one_over_sqrt_2, Complex.neg one_over_sqrt_2)
    | Rm m -> Complex.(one, zero, zero, exp (div (mul two_pi i) (pow complex_two {re = float_of_int m; im = 0.0})))

  let apply_gate (g : Circuit.gate) (state : qstate) = 
    let iterations = Array.length state / 2 in
    let (a, b, c, d) = matrix_for_gate g.kind in
    let (A t) = g.target in

    for i = 0 to iterations - 1 do
      let (firstIndex, secondIndex) = iterationIndices i t in
      let first = Array.get state firstIndex in
      let second = Array.get state secondIndex in
      let check = controls_check firstIndex g.controls in
      if check then begin
        Array.set state firstIndex (Complex.add (Complex.mul a first) (Complex.mul b second));
        Array.set state secondIndex (Complex.add (Complex.mul c first) (Complex.mul d second))
      end
    done

  let repr x = x
end

(** Naive Simulator *)
module NaiveSimulator = Make(NaiveBackend)

