open Circuit
open Naive

let superposition n = {qbits = n; gates = List.init n (fun i -> {target = (A i); kind = H; controls = []})}

let entanglement n = {qbits = n; gates = List.cons ({target = (A 0); kind = H; controls = []}) (List.init (n-1) (fun i -> {target = (A (i+1)); kind = X; controls = [A (0)]}))}

let superpos_res = NaiveSimulator.run (superposition 10)
let entanglement_res = NaiveSimulator.run (entanglement 3)



