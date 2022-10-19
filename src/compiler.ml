open Utils

(* type mapping = ((string * int) * int) list *)

(* Quantum register location and size within address space *)
type register = {
  (* Address of the first qubit *)
  first: int;
  (* Size of the register *)
  size: int;
}

type addresses = {
  (* Register name bindings *)
  mapping: (string, register) Hashtbl.t;
  (* Total size of the address space *)
  size: int;
}

let rec resolve_qaddresses' (f : Qasm.stmt list) (addrs : addresses) : addresses =
  match f with
  | [] -> addrs
  | Qasm.Qreg (Qasm.Id name, Qasm.Nnint sz) :: tail ->
    let reg : register = {
      first = addrs.size;
      size = sz;
    } in
    (* FIXME: throw an error if there is a redundant register name *)
    let _ = Hashtbl.add addrs.mapping name reg in
    let addrs' = {
      mapping = addrs.mapping;
      size = addrs.size + sz;
    } in
    resolve_qaddresses' tail addrs'
  | _ :: tail -> resolve_qaddresses' tail addrs

(* Produce an address space map for the program *)
let resolve_qaddresses (f : Qasm.stmt list) : addresses =
  resolve_qaddresses' f {
    mapping = Hashtbl.create 8;
    size = 0;
  }

(* TODO: turn a uop into a Circuit.gate *)
let translate_gate addrs (_uop : Qasm.uop) = todo ()

(* Second pass: filter gates from Qasm AST *)
let rec compile_gates (f : Qasm.stmt list) (addrs : addresses) =
  match f with
  | [] -> []
  | Qasm.Qop (Qasm.Q_uop uop) :: tail -> (translate_gate addrs uop) :: (compile_gates tail addrs)
  (* In this pass, ignore the `qreg` declarations *)
  | Qasm.Qreg _ :: tail -> (compile_gates tail addrs)
  | _ -> failwith "Unsupported instruction"


let compile (prog : Qasm.t) : Circuit.t =
  let addrs = resolve_qaddresses prog.body in
  let gates = compile_gates prog.body addrs in
  { qbits = addrs.size; gates; }
