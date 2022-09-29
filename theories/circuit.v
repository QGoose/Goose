(** * Quantum Circuit Intermediate Representation

  A simplified representation of Quantum Circuits
*)

Require Import Reals ZArith List.


Inductive adr : Type := Adr (_ : nat).

(**
  Kind of Gates.
  
  For the sake of simplicitiy, we pick a small
  set of gates that is not enough to express arbitrary
  quantum circuits.
*)
Inductive gate_kind : Type :=
  | X (* Not Gate *)
  | H (* Hadamard Gate *)
  | Z (* Phase Gate *)
  .

(**
  Generic Gates.

  A gate has a target qubit and a set of set of controls
*)
Record gate : Type := {
  (* Target qubit address *)
  target : adr;
  (* Which gate do we apply? *)
  kind : gate_kind;
  controls : list adr;
}.

(**
  Circuits.
  
  A quantum circuit is defined
  by its number a qubits
  and an (ordered) list of gates
*)
Record circuit : Type := {
  (* Total number of qubits *)
  qbits : nat;
  (* The actual circuit *)
  gates : list gate;
}.

Definition prob := { r : R | (0 <= r <= 1)%R }.

Record complex : Type := {
  re : R;
  im : R;
}.

Definition cadd (c1 c2 : complex) := {|
  re := (re c1 + re c2)%R;
  im := (im c1 + im c2)%R;
|}.

Definition csub (c1 c2 : complex) := {|
  re := (re c1 - re c2)%R;
  im := (im c1 - im c2)%R;
|}.

Definition cmul (c1 c2 : complex) := {|
  re := ((re c1 * re c2) - (im c1 * im c2))%R;
  im := ((re c1 * im c2) + (im c1 * re c2))%R;
|}.

Definition cscale (r : R) (c : complex) := {|
  re := r * re c;
  im := r * im c;
|}.

Definition fin (N : nat) : Type :=
  { x : nat | x < N }.

Definition qstate (qbits : nat) : Type :=
  fin (2 ^ qbits) -> complex.

(**
  Small step semantics for gate kind
*)
Inductive gate_step (qbits : nat) 
: gate_kind -> qstate qbits -> prob -> qstate qbits -> Prop :=
  
  | gs_X s1 p s2 : (* ... *) gate_step qbits X s1 p s2
  | gs_H s1 p s2 : (* ... *) gate_step qbits H s1 p s2
  | gs_Z s1 p s2 : (* ... *) gate_step qbits Z s1 p s2
  .