(** * Quantum Circuit Intermediate Representation

  A simplified representation of Quantum Circuits
*)

type adr = A of int

(**
  Kind of Gates.
  
  For the sake of simplicitiy, we pick a small
  set of gates that is not enough to express arbitrary
  quantum circuits.
*)
type gate_kind =
  | X (* Not Gate *)
  | H (* Hadamard Gate *)
  | Z (* Phase Gate *)
  (* Rm of int *)

(**
  Generic Gates.

  A gate has a target qubit and a set of set of controls
*)
type gate = {
  (* Target qubit address *)
  target : adr;
  (* Which gate do we apply? *)
  kind : gate_kind;
  controls : adr list;
}

(**
  Circuits.
  
  A quantum circuit is defined
  by its number a qubits
  and an (ordered) list of gates
*)
type t = {
  (* Total number of qubits *)
  qbits : int;
  (* The actual circuit *)
  gates : gate list;
}