type qstate = Complex.t array

let init qbits =
  let len = 1 lsl qbits in
  (* All-zero state *)
  let state = Array.make len Complex.zero in
  (* effectfully set the |00...0> entry to 1 *)
  Array.set state 0 Complex.one;
  state

let apply_gate (_ : Circuit.gate) (_ : qstate) = Utils.todo ()

let repr x = x
