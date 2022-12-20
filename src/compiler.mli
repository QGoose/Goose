(** A compiler from Qasm to circuits. *)

(** Compiles an OpenQASM program to a circuit. *)
val compile : Qasm.t -> Circuit.t