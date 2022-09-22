module type BACKEND = sig
  type qstate
  type cstate
  val pp_qstate : Format.formatter -> qstate -> unit
  val pp_cstate : Format.formatter -> cstate -> unit
end

module Make(B : BACKEND) : sig
(*
  The inputs have to be fixed in the code
  TODO: static analysis to figure out if the inputs are not fixed
*)
  val run : Qasm.prog -> B.cstate
end = struct
  let run _ = failwith "todo"
end

(*
qreg x[10] --> there exists 10 qubits called  x0...x9
creg y[10] --> there exists 10 bits called    x0...x9
*)

(* A demo simulation backend to implement `simulate` *)
(* Naive in two ways: it's a very poor implementation, and it's probably not nice OCaml. *)
module Naive_simulation = struct
  type quantum_state = Complex.t array

  (* I guess this should be impure because `array` is *)
  let apply_gate (_ : Circuit.gate) (_ : quantum_state) = Utils.todo ()

  let init_state (circ : Circuit.circuit) : quantum_state =
    (* state array should be 2^n entries long *)
    let len = Int.shift_left 0 circ.qbits in
    (* All-zero state *)
    let state = Array.make len Complex.zero in
    (* effectfully set the |00...0> entry to 1 *)
    Array.set state 0 Complex.one;
    state

  let output_state (circ: Circuit.circuit) : quantum_state =
    let state = init_state (circ) in
    List.iter (fun g -> apply_gate g state) circ.gates;
    state

end

(* TODO: some kind of top-level entry point for simulation. *)
(* Probably has a more interesting type than `circuit -> state`. *)
let simulate (circ : Circuit.circuit) = Naive_simulation.output_state circ
