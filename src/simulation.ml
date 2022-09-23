open Circuit

module type BACKEND = sig
  type qstate
  val init : int -> qstate
  
  val apply_gate : Circuit.gate -> qstate -> unit

  val repr : qstate -> Complex.t array
end

module Make(B : BACKEND) : sig
  val run : Circuit.t -> B.qstate
end = struct
  let run circ =
    let state = B.init circ.qbits in
    List.iter (fun g -> B.apply_gate g state) circ.gates;
    state
end

module NaiveBackend : BACKEND = struct
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
end

module NaiveSimulator = Make(NaiveBackend)

