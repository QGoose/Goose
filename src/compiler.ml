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

let new_addresses (_ : unit) : addresses = {
  mapping = Hashtbl.create 8;
  size = 0;
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

let rec collect_gate_decls' (f : Qasm.stmt list) decls = match f with
  | [] -> ()
  | GateDecl (name, _params, _targets, _body) :: tail ->
    let body = () in
    let _ = Hashtbl.add decls name body in
    collect_gate_decls' tail decls
  | _ :: tail -> collect_gate_decls' tail decls

let collect_gate_decls (f : Qasm.stmt list) =
  let gate_decls = Hashtbl.create 8 in
  let _ = collect_gate_decls' f gate_decls in
  gate_decls

let eval_float (_expr : Qasm.expr) : float = todo ()

(* TODO: turn a uop into a Circuit.gate *)
let translate_gate addrs (uop : Qasm.uop) : Circuit.gate list = match uop with
  | U (theta :: phi :: lambda :: [], arg) ->
    let kind : Circuit.gate_kind = U {
        theta = eval_float theta;
        phi = eval_float phi;
        lambda = eval_float lambda;
      } in
    let tgt = match arg with
      (* TODO: Act on all qubits in a register? *)
      | A_id (Id _tgt_reg, None) -> failwith "Unimplemented: map gate over register"
      (* Act on a single qubit in a register at the given offset *)
      | A_id (Id tgt_reg, Some tgt_offset) -> resolve addrs tgt_reg (Qasm.int_of_nnint tgt_offset)
    in
    let (gate : Circuit.gate) = {
      target = tgt;
      kind = kind;
      controls = [];
    }
    in [gate]
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

(* starting over *)

type gate = unit

type environment = {
  (* Register name bindings *)
  addrs: addresses;
  (* Gate abstractions *)
  funcs: (string, gate) Hashtbl.t;
}

(*  Create a new empty environment *)
let new_environment (_ : unit) : environment = {
  addrs = new_addresses ();
  funcs = Hashtbl.create 8;
}

(* TODO: rename *)
type compile_state = {
  env: environment;
  gates: Circuit.gate list;
}

let compile_stmt (env : compile_state) (stmt : Qasm.stmt) : compile_state = match stmt with
  | Qasm.Qreg (Qasm.Id name, Qasm.Nnint sz) -> todo ()
  | GateDecl (name, _params, _targets, _body) -> todo ()
  | Qasm.Qop (Qasm.Q_uop uop) -> todo ()
  | _ -> todo ()

let compile' (prog : Qasm.t) : Circuit.t =
  let init_state = {
    env = new_environment ();
    gates = [];
  } in
  let (out_state : compile_state) = List.fold_left compile_stmt init_state prog.body in
  {
    qbits = out_state.env.addrs.size;
    gates = out_state.gates;
  }
