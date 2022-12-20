(** A collection of examples using circuits expressed in {!Circuit.t} *)

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

let groversDiffusionList n = List.concat [
  List.init n (fun i -> {target = (A i); kind = H; controls = []});
  List.init n (fun i -> {target = (A i); kind = X; controls = []});
  [{target = (A (n-1)); kind = Z; controls = (List.init (n-1) (fun i -> (A i)))}];
  List.init n (fun i -> {target = (A i); kind = X; controls = []});
  List.init n (fun i -> {target = (A i); kind = H; controls = []})
]

let groversOracleList n offQubits = List.concat [
  List.map (fun q -> {target = (A q); kind = X; controls = []}) offQubits;
  [{target = (A (n-1)); kind = Z; controls = (List.init (n-1) (fun i -> (A i)))}];
  List.map (fun q -> {target = (A q); kind = X; controls = []}) offQubits
]

let repeat ls n =
  let rec f l = function
      | 0 -> l
      | n -> f (List.rev_append ls l) (n-1) in
  List.rev (f [] n)

let groversSearch n offQubits iterations = { (* The off-qubits are indexed from the right *)
  qbits = n;
  gates = List.concat [
    List.init n (fun i -> {target = (A i); kind = H; controls = []});
    repeat (List.concat [(groversDiffusionList n); (groversOracleList n offQubits)]) iterations
  ]
}

let superpos_res = NaiveSimulator.run (superposition 10)
let entanglement_res = NaiveSimulator.run (entanglement 3)

let entanglement_se_res = SE_Engine.run (entanglement 3)

let qft3_res = NaiveSimulator.run (qft 3);;

let qft3_se_res = SE_Engine.run (qft 3);;

let grovers_res = NaiveSimulator.run (groversSearch 3 [0; 1] 3);;
let grovers_se_res = SE_Engine.run (groversSearch 3 [0; 1] 3);;