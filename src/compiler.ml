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

(* let rec collect_gate_decls' (f : Qasm.stmt list) decls = match f with
   | [] -> ()
   | GateDecl (name, _params, _targets, _body) :: tail ->
    let body = () in
    let _ = Hashtbl.add decls name body in
    collect_gate_decls' tail decls
   | _ :: tail -> collect_gate_decls' tail decls *)

(* let collect_gate_decls (f : Qasm.stmt list) =
   let gate_decls = Hashtbl.create 8 in
   let _ = collect_gate_decls' f gate_decls in
   gate_decls *)

let rec eval_float (expr : Qasm.expr) : float = match expr with
  | E_cst x -> x
  (* TODO *)
  | E_int Qasm.Nnint n -> float_of_int n
  | E_Pi -> Float.pi
  (* TODO *)
  | E_id _id -> failwith "unimplemented: eval `id`"
  | E_bop (binop, e1, e2) -> let binop = match binop with
      | ADD -> ( +. )
      | MUL -> ( *. )
      | SUB -> ( -. )
      | DIV -> ( /. )
      | POW -> ( ** )
    in binop (eval_float e1) (eval_float e2)
  | E_uop (uop, e') -> let uop = match uop with
      | SIN -> Float.sin
      | COS -> Float.cos
      | TAN -> Float.tan
      | EXP -> Float.exp
      | NEG -> Float.neg
      | LN -> Float.log
      | SQRT -> Float.sqrt
      | INV -> ( /. ) 1.
    in uop (eval_float e')

(* TODO: this should be a lower-level representation than that in qasm.ml *)
type gate_abstraction = {
  params : Qasm.id list;
  qargs : Qasm.id list;
  gates : Qasm.gop list;
}

type environment = {
  (* Register name bindings *)
  addrs: addresses;
  (* Gate abstractions: map abstract gate names to their definitions *)
  funcs: (Qasm.id, gate_abstraction) Hashtbl.t;
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

(* Compiler register declaration *)
let compile_reg_decl (addrs : addresses) (name : Qasm.id) (sz : Qasm.nnint) : addresses =
  (* Unwrap the arguments *)
  let Qasm.Id name = name in
  let Qasm.Nnint sz = sz in
  (* Create the new register *)
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
  addrs'

(* Compile gate declaration, add it to the environment *)
let compile_gate_decl (cs : compile_state) (decl : Qasm.gate_decl) : compile_state =
  let abstract_gate = {
    params = decl.params;
    qargs = decl.qargs;
    gates = decl.gates;
  } in
  let _ = Hashtbl.add cs.env.funcs decl.name abstract_gate in
  cs

let subst_args (_abstract_gate : gate_abstraction) (_args1 : Qasm.expr list) (_args2 : Qasm.arg list) : Qasm.gop list = todo ()

(* Turn a uop into (possibly a list of) Circuit.gate *)
let rec compile_uop (env: environment) (uop : Qasm.uop) : Circuit.gate list = match uop with
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
      | A_id (Id tgt_reg, Some tgt_offset) -> resolve env.addrs tgt_reg (Qasm.int_of_nnint tgt_offset)
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
    let tgt = resolve env.addrs tgt_reg (Qasm.int_of_nnint tgt_offset) in
    let src = resolve env.addrs src_reg (Qasm.int_of_nnint src_offset) in
    {
      target = tgt;
      kind = X;
      controls = [src];
    } :: []
  (* TODO: actual error message: trying to map over something that can't be mapped over. *)
  | CX _ -> failwith "Illegal control operation"
  | App (name, args1, args2) ->
    let abstract_gate = Hashtbl.find env.funcs name in
    subst_args abstract_gate args1 args2 |>
    List.map (compile_gop env) |>
    List.flatten

and compile_gop (env : environment) (gop : Qasm.gop) : Circuit.gate list = match gop with
  (* Simply ignore barriers *)
  | G_barrier _ -> []
  | G_uop uop -> compile_uop env uop

let compile_stmt (cs : compile_state) (stmt : Qasm.stmt) : compile_state = match stmt with
  (* Compile register declaration *)
  | Qasm.Qreg (name, sz) -> let addrs' = compile_reg_decl cs.env.addrs name sz in
    {
      env = {
        funcs = cs.env.funcs;
        (* Only update the addresses! *)
        addrs = addrs';
      };
      gates = cs.gates;
    }
  (* Compile gate declaration *)
  | GateDecl decl -> compile_gate_decl cs decl
  (* Compile uop *)
  | Qasm.Qop (Qasm.Q_uop uop) -> let new_gates = compile_uop cs.env uop in
    {
      env = cs.env;
      gates = cs.gates @ new_gates;
    }
  (* FIXME Ignore everything else for now *)
  | _ -> todo ()

let compile (prog : Qasm.t) : Circuit.t =
  let init_state = {
    env = new_environment ();
    gates = [];
  } in
  let (out_state : compile_state) = List.fold_left compile_stmt init_state prog.body in
  {
    qbits = out_state.env.addrs.size;
    gates = out_state.gates;
  }
