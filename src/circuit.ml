(** A simplified representation of Quantum Circuits. *)

type adr = A of int

let string_of_adr (A n : adr) = string_of_int n

(** An arbitrary single-qubit gate, parametrized as in OpenQASM *)
type arb_sq_gate = {
  theta : float;
  phi : float;
  lambda : float;
}

let string_of_arb_sq_gate (gate : arb_sq_gate) =
  Printf.sprintf "U(%f,%f,%f)" gate.theta gate.phi gate.lambda

(** Kind of Gates. *)
type gate_kind =
  | X (* Not Gate *)
  | H (* Hadamard Gate *)
  | Z (* Phase Gate *)
  | U of arb_sq_gate
  | Rm of int

let string_of_gate_kind (kind : gate_kind) =
  match kind with
  | X -> "X"
  | H -> "H"
  | Z -> "Z"
  | U gate -> string_of_arb_sq_gate gate
  | Rm _frac -> failwith "unimplemented"

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

let string_of_gate (gate : gate) =
  let base = Printf.sprintf "%s %s" (string_of_gate_kind gate.kind) (string_of_adr gate.target) in
  let rec string_of_controls (controls : adr list) = match controls with
    | [] -> base
    | head :: tail -> Printf.sprintf "C %s %s" (string_of_adr head) (string_of_controls tail)
  in string_of_controls gate.controls

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

let string_of_circuit (circ : t) : string =
  Printf.sprintf "%d qubits\n%s" circ.qbits (String.concat "\n" (List.map string_of_gate circ.gates))
