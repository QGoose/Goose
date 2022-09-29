open Circuit

module type BACKEND = sig
  type qstate
  val init : int -> qstate

  val apply_gate : Circuit.gate -> qstate -> unit
end

module type CBACKEND = sig
  include BACKEND
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
