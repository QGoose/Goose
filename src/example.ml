open Circuit
open Naive
open Se

(* n-qubit uniform superposition state. *)
let superposition n = {qbits = n; gates = List.init n (fun i -> {target = (A i); kind = H; controls = []})}

(* n-qubit GHZ state. *)
let entanglement n = {
  qbits = n;
  gates = List.cons ({
    target = A 0;
    kind = H;
    controls = []
  }) (List.init (n - 1) (fun i -> {
    target = A (i + 1);
    kind = X;
    controls = [A 0]
  }))
}

(* n-qubit Quantum Fourier Transform. *)
let qft n = {
  qbits = n;
  gates = List.concat (List.init n (fun i -> 
    List.cons 
    ({target = (A i); kind = H; controls = []}) 
    (List.init (n-i-1) (fun j -> {target = (A i); kind = Rm (j+2); controls = [A (j+i+1)]}))));
}

let superpos_res = NaiveSimulator.run (superposition 10)
let entanglement_res = NaiveSimulator.run (entanglement 3)

let entanglement_se_res = SE_Engine.run (entanglement 3)

let qft3_res = NaiveSimulator.run (qft 3);;

let qft3_se_res = SE_Engine.run (qft 3);;