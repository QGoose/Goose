(* Almost *certainly* not the representation we want, *)
(* since it's strictly less expressive than the OpenQASM gates. *)
type sq_gate_kind = X | Y | Z | T | TDag | S | SDag

type sq_gate = {
  (* Target qubit address *)
  tgt : int;
  (* Which gate do we apply? *)
  kind : sq_gate_kind
}

type ctrl_gate = {
  (* Source qubit address *)
  src : int;
  (* Gate applied at target *)
  op : sq_gate;
}

(* Consider this a *sketch* of the show to tell you what it's supposed to represent. *)
(* This might not be interface we actually want. *)
type gate = Sq_gate of sq_gate | Ctrl_gate of ctrl_gate

(* A circuit representation that's really a mildly desugared version of QASM. *)
(* No named registers, no-op instructions, measurement, etc. *)
(* N.b. measurement *is* syntactic sugar! *)
(* N.b. All of these types are *sketches* to tell you what they're supposed to represent. *)
(* They might not be the interface we really want. *)
(* How do I do block comments in this silly language? *)
type circuit = {
  (* Total number of qubits *)
  qbits : int;
  (* Total number of cbits *)
  cbits : int;
  gates : gate list;
}

(* TODO: should return a `circuit` *)
let lower_ast _ = Utils.todo ()
