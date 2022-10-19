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

(* TODO: make this namespaced by a module, where `addresses` becomes `Addresses.t` or whatever the convention is. *)
let resolve (addrs : addresses) (name : string) (offset : int) : Circuit.adr =
  (* Why doesn't this function return a `register option`? *)
  let (reg : register) = Hashtbl.find addrs.mapping name in
  if offset >= reg.size then
    (* TODO: real error message *)
    failwith "Illegal register offset"
  else
    A (reg.first + offset)

let rec alloc_qaddresses' (f : Qasm.stmt list) (addrs : addresses) : addresses =
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
    alloc_qaddresses' tail addrs'
  | _ :: tail -> alloc_qaddresses' tail addrs

(* Produce an address space map for the program *)
let alloc_qaddresses (f : Qasm.stmt list) : addresses =
  alloc_qaddresses' f {
    mapping = Hashtbl.create 8;
    size = 0;
  }

(* TODO: turn a uop into a Circuit.gate *)
let translate_gate addrs (uop : Qasm.uop) : Circuit.gate list = match uop with
  | U (_theta :: _phi :: _lambda :: [], _) -> todo ()
  (* FIXME: It seems like this shouldn't be a semantic error but a parser error. *)
  | U (_, _) -> failwith "Illegal arbitrary single qubit unitary"
  | CX (A_id (Id _src_reg, None), A_id (Id _tgt_reg, None)) -> todo ()
  | CX (A_id (Id src_reg, Some src_offset), A_id (Id tgt_reg, Some tgt_offset)) ->
    let tgt = resolve addrs tgt_reg (Qasm.int_of_nnint tgt_offset) in
    let src = resolve addrs src_reg (Qasm.int_of_nnint src_offset) in
    {
      target = tgt;
      kind = X;
      controls = [src];
    } :: []
  (* TODO: actual error message: trying to map over something that can't be mapped over. *)
  | CX _ -> failwith "Illegal control operation"
  | App (_, _, _) -> todo()

(* Second pass: filter gates from Qasm AST *)
let rec compile_gates (f : Qasm.stmt list) (addrs : addresses) =
  match f with
  | [] -> []
  | Qasm.Qop (Qasm.Q_uop uop) :: tail -> (translate_gate addrs uop) @ (compile_gates tail addrs)
  (* In this pass, ignore the `qreg` declarations *)
  | Qasm.Qreg _ :: tail -> (compile_gates tail addrs)
  | _ -> failwith "Unsupported instruction"


let compile (prog : Qasm.t) : Circuit.t =
  let addrs = alloc_qaddresses prog.body in
  let gates = compile_gates prog.body addrs in
  { qbits = addrs.size; gates; }
