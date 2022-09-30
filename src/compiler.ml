open Utils

type mapping = ((string * int) * int) list

(* First pass: remap named registers to a single address space *)
(* Return a pair of size of address space and register mapping *)
let rec resolve_qaddresses (f : Qasm.stmt list) (count : int) : int * mapping =
  match f with
  | [] -> (0, [])
  | Qasm.Qreg (Qasm.Id name, Qasm.Nnint sz) :: tail ->
    let count1 = count + sz in
    let map1 = List.init sz (fun i -> ((name, i), count + i)) in
    let (count2, map2) = resolve_qaddresses tail count1 in
    (count2, map1 @ map2)
  | _ :: tail -> resolve_qaddresses tail count

(* TODO: turn a uop into a Circuit.gate *)
let translate_gate _address_for (_uop : Qasm.uop) = todo ()

(* Second pass: filter gates from Qasm AST *)
let rec compile_gates (f : Qasm.stmt list) address_for =
  match f with
  | [] -> []
  | Qasm.Qop (Qasm.Q_uop uop) :: tail -> (translate_gate address_for uop) :: (compile_gates tail address_for)
  (* In this pass, ignore the `qreg` declarations *)
  | Qasm.Qreg _ :: tail -> (compile_gates tail address_for)
  | _ -> failwith "Unsupported instruction"


let compile (prog : Qasm.t) : Circuit.t =
  let (qbits, mapping) = resolve_qaddresses prog.body 0 in
  let gates = compile_gates prog.body mapping in
  { qbits; gates; }
