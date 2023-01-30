open Utils

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

let rec subst_args_expr (param_subs : (Qasm.id, Qasm.expr) Hashtbl.t)
    (param : Qasm.expr) : Qasm.expr = match param with
  | E_id id -> (match Hashtbl.find_opt param_subs id with
      | Some e' -> e'
      | None -> Qasm.E_id id)
  | E_bop (binop, e1, e2) ->
    let e1' = subst_args_expr param_subs e1 in
    let e2' = subst_args_expr param_subs e2 in
    E_bop (binop, e1', e2')
  | E_uop (unop, e) ->
    let e' = subst_args_expr param_subs e in
    E_uop (unop, e')
  (* Otherwise, do nothing *)
  | e -> e

let subst_args_arg (qarg_subs : (Qasm.id, Qasm.arg) Hashtbl.t) (arg : Qasm.arg) : Qasm.arg =
  let A_id (id, idx) = arg in
  match Hashtbl.find_opt qarg_subs id with
  | Some A_id (id', idx') ->
    (* TODO: Open question: what are the semantics of passing an indexed register into a gate that indexes that argument again? *)
    let idx'' = match (idx, idx') with
      | (None, None) -> None
      | (Some idx', None) | (None, Some idx') -> Some idx'
      | (Some _, Some _) -> failwith "The semantics of this program are unclear."
    in A_id (id', idx'')
  | None -> arg

let subst_args_uop
    (param_subs : (Qasm.id, Qasm.expr) Hashtbl.t)
    (qarg_subs : (Qasm.id, Qasm.arg) Hashtbl.t)
    (uop : Qasm.uop): Qasm.uop = match uop with
  | U (exprs, arg) ->
    let exprs' = List.map (subst_args_expr param_subs) exprs in
    let arg' = subst_args_arg qarg_subs arg in
    U (exprs', arg')
  | CX (arg1, arg2) -> CX (subst_args_arg qarg_subs arg1, subst_args_arg qarg_subs arg2)
  | App (name, inner_params, inner_qargs) ->
    let inner_params' = List.map (subst_args_expr param_subs) inner_params in
    let inner_qargs' = List.map (subst_args_arg qarg_subs) inner_qargs in
    App (name, inner_params', inner_qargs')

let subst_args_gop
    (param_subs : (Qasm.id, Qasm.expr) Hashtbl.t)
    (qarg_subs : (Qasm.id, Qasm.arg) Hashtbl.t)
    (gop : Qasm.gop): Qasm.gop =
  match gop with
  | G_uop uop -> G_uop (subst_args_uop param_subs qarg_subs uop)
  | x -> x

let subst_args (abstract_gate : gate_abstraction) (params : Qasm.expr list) (qargs : Qasm.arg list) : Qasm.gop list =
  let param_subs = Seq.zip (List.to_seq abstract_gate.params) (List.to_seq params) |> Hashtbl.of_seq in
  let qarg_subs = Seq.zip (List.to_seq abstract_gate.qargs) (List.to_seq qargs) |> Hashtbl.of_seq in
  List.map (subst_args_gop param_subs qarg_subs) abstract_gate.gates

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
