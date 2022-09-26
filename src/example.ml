open Circuit
open Naive

let superposition n = {qbits = n; gates = List.init n (fun i -> {target = (A i); kind = H; controls = []})}

let entanglement n = {qbits = n; gates = List.cons ({target = (A 0); kind = H; controls = []}) (List.init (n-1) (fun i -> {target = (A (i+1)); kind = X; controls = [A (0)]}))}

let qft = {
  qbits = 2;
  gates = [
    {target = A 0; kind = H; controls = []};
    {target = A 0; kind = Rm 2; controls = [A 1]};
    {target = A 1; kind = H; controls = []};
  ]
}

let superpos_res = NaiveSimulator.run (superposition 10)
let entanglement_res = NaiveSimulator.run (entanglement 3)



