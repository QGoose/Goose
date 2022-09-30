open Utils

(* First pass: remap named registers to a single address space *)
(* Return a pair of size of address space and register mapping *)
let rec resolve_qaddresses (f : Qasm.stmt list) (count : int) =
  match f with
  | [] -> (0, fun n _ -> failwith @@ Printf.sprintf "qreg '%s' does not exist" n)
  | Qasm.Qreg (Qasm.Id name, Qasm.Nnint sz) :: tail ->
    let count' = count + sz in
    let address_for = (fun (name' : string) (idx : int) ->
        if name = name'
        then if idx < sz
          then count + idx
          else failwith @@ Printf.sprintf "index %d is out of bounds, qreg '%s' was previously declared with size %d" idx name sz
        else
          let (_, address_for') = (resolve_qaddresses tail count') in
          address_for' name' idx
      ) in
    (count', address_for)
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
  let (sz, address_for) = resolve_qaddresses prog.body 0 in
  let gates = compile_gates prog.body address_for in
  {
    qbits = sz;
    gates = gates;
  }
