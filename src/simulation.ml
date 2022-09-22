let simulate _ = Utils.todo ()

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
