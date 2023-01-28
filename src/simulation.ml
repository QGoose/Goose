(** Interface of simulators for quantum circuits *)

open Circuit

(** A generic backend for quantum circuit simulation. *)
module type BACKEND = sig
  type qstate
  val init : int -> qstate

  val apply_gate : Circuit.gate -> qstate -> unit

  val print_state: out_channel -> qstate -> unit
end

(** Given a [BACKEND], produces a quantum circuit simulator by defining the [run] function. *)
module Make(B : BACKEND) : sig
  val run : Circuit.t -> B.qstate
end = struct
  let run circ =
    let state = B.init circ.qbits in
    List.iter (fun g -> B.apply_gate g state) circ.gates;
    state
end
