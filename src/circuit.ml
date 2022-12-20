(** A simplified representation of Quantum Circuits. *)

type adr = A of int

(** An arbitrary single-qubit gate, parametrized as in OpenQASM *)
type arb_sq_gate = {
  theta : float;
  phi : float;
  lambda : float;
}

(** Kind of Gates. *)
type gate_kind =
  | X (* Not Gate *)
  | H (* Hadamard Gate *)
  | Z (* Phase Gate *)
  | U of arb_sq_gate
  | Rm of int

(** Generic Gates.
    A gate has a target qubit and a set of set of controls.
*)
type gate = {
  (* Target qubit address *)
  target : adr;
  (* Which gate do we apply? *)
  kind : gate_kind;
  controls : adr list;
}

(** Circuits.
   A quantum circuit is defined
   by its number a qubits
   and an (ordered) list of gates.
*)
type t = {
  (* Total number of qubits *)
  qbits : int;
  (* The actual circuit *)
  gates : gate list;
}
